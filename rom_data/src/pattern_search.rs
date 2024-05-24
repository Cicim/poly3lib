pub enum SearchPart {
    Exact(&'static [u8]),
    Skip(u32),
    Save(u32),
}

pub struct SearchPattern {
    parts: &'static [SearchPart],
}

#[derive(Debug)]
pub struct SearchResult {
    pub offset: usize,
    pub groups: Vec<(usize, Vec<u8>)>,
}

impl std::fmt::Debug for SearchPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "search_pattern!(")?;

        for part in self.parts {
            match part {
                SearchPart::Exact(bytes) => {
                    write!(f, "{{ ")?;
                    for byte in bytes.iter() {
                        write!(f, "{:#02x} ", byte)?;
                    }
                    write!(f, "}}")?;
                }
                SearchPart::Skip(count) => {
                    write!(f, "{{ skip {} }}", count)?;
                }
                SearchPart::Save(count) => {
                    write!(f, "{{ save {} }}", count)?;
                }
            }
        }

        write!(f, ")")
    }
}

impl SearchPattern {
    pub const fn new(parts: &'static [SearchPart]) -> Self {
        assert!(
            matches!(parts[0], SearchPart::Exact(_)),
            "The first part of a search pattern must be an exact match."
        );

        Self { parts }
    }

    pub fn len(&self) -> usize {
        self.parts
            .iter()
            .map(|part| match part {
                SearchPart::Exact(bytes) => bytes.len(),
                SearchPart::Skip(count) => *count as usize,
                SearchPart::Save(count) => *count as usize,
            })
            .sum()
    }

    /// Find the first offset satisfying the search pattern.
    pub fn find(&self, haystack: &[u8]) -> Option<SearchResult> {
        // The first part is always an exact match
        let first_match = match &self.parts[0] {
            SearchPart::Exact(bytes) => bytes,
            _ => unreachable!(),
        };

        let first_match_iter = (0..haystack.len()).filter_map(|i| {
            if haystack[i..].starts_with(first_match) {
                Some(i)
            } else {
                None
            }
        });

        for candidate_start in first_match_iter {
            let mut offset = candidate_start + first_match.len();
            let mut groups = Vec::new();

            for part in &self.parts[1..] {
                match part {
                    SearchPart::Exact(bytes) => {
                        if haystack[offset..].starts_with(bytes) {
                            offset += bytes.len();
                        } else {
                            break;
                        }
                    }
                    SearchPart::Skip(count) => {
                        offset += *count as usize;
                    }
                    SearchPart::Save(count) => {
                        let group = haystack[offset..offset + *count as usize].to_vec();
                        groups.push((offset, group));
                        offset += *count as usize;
                    }
                }
            }

            return Some(SearchResult { offset, groups });
        }

        None
    }
}

#[macro_export]
/// Macro to define a search pattern.
///
/// Must start with an exact match.
///
/// # Example
/// ```
/// use rom_data::{SearchPattern, search_pattern};
///
/// const PATTERN: SearchPattern = search_pattern!({ 0x12 0x34 0x56 }{ skip 2 }{ save 4 });
/// ```
macro_rules! search_pattern {
    // Match the complete pattern and process each part
    ({ $($exact:literal)+ } $($part:tt)+) => {
        SearchPattern::new(&[
            $crate::SearchPart::Exact(&[
                $($exact),*
            ]),
            $(search_pattern!($part)),*
        ])
    };

    ({ $($bytes:literal)+ }) => {
        $crate::SearchPart::Exact(&[
            $($bytes),*
        ])
    };

    // Parse `Skip` parts
    ({ skip $skip:literal }) => {
        $crate::SearchPart::Skip($skip)
    };

    // Parse `Save` parts
    ({ save $size:literal }) => {
        $crate::SearchPart::Save($size)
    };
}
