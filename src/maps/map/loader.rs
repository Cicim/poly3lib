//! Detecting and loading map groups from ROM.

use std::collections::HashMap;

use rom_data::{
    types::{RomPointer, RomSizedType},
    Offset, RomIoError,
};

use crate::{maps::layout::MapLayout, Rom, RomTable};

use super::{scripts::ON_RETURN_TO_FIELD, MapGroups, MapHeader};

const DEBUG_PRINT_MAP_LOADING: bool = false;

/// Find all the map groups in the ROM.
pub(super) fn find_map_groups_in_rom(rom: &Rom) -> Result<MapGroups, RomIoError> {
    // Find the reference to the map groups table
    let table_offset = find_map_groups_reference_in_rom(&rom)?;

    // group_id -> (group_offset, group_size)
    let mut group_footprints = HashMap::with_capacity(256);
    let mut group_ids = Vec::with_capacity(256);
    // Only for debugging purposes
    let mut skipped = 0;

    // Find all the groups containing some valid map headers.
    for group_id in 0..256 {
        let group_reference = table_offset + 0x4 * group_id;
        match rom.data.read_offset(group_reference) {
            Ok(group_offset) => {
                // Loop over the next 256 maps in the group
                // to obtain how many of them look like valid map headers
                let mut last_header = 0;
                let mut valid_count = 0;

                // Only for debugging purposes
                let mut trace1 = "".to_owned();
                let mut trace2 = "".to_owned();

                for i in 0..256 {
                    let header_pointer = group_offset + 0x4 * i;

                    let problem = get_map_header_pointer_problem(rom, header_pointer);
                    if problem.is_ok() {
                        last_header = i;
                        valid_count += 1;
                    }

                    if DEBUG_PRINT_MAP_LOADING {
                        let trace = if i < 128 { &mut trace1 } else { &mut trace2 };
                        trace.push_str(&format!("{}", problem));
                    }
                }

                if valid_count == 0 {
                    skipped += 1;
                    continue;
                }

                if DEBUG_PRINT_MAP_LOADING {
                    use colored::Colorize;

                    if skipped > 0 {
                        println!(
                            "{}",
                            format!(
                                "    ⋮     Skipped {} groups ({}-{})",
                                skipped,
                                group_id - skipped,
                                group_id - 1
                            )
                            .red()
                        );
                        skipped = 0;
                    }

                    println!("{}", format!(
                            "{:08X}: Possible group {:>3} found starting at ${:07X} with up to {:>3} headers {} verified",
                            group_reference,
                            group_id,
                            group_offset,
                            last_header + 1,
                            valid_count,
                        ).green());
                    println!("     {}\n     {}", trace1, trace2);
                }

                group_ids.push(group_id);
                group_footprints.insert(group_id, (group_offset, last_header + 1));
            }
            Err(_) => skipped += 1,
        }
    }

    if DEBUG_PRINT_MAP_LOADING {
        println!();
        println!();
        println!();
        println!();
    }

    for current_group in group_ids.iter() {
        // Check if there is any group (that is not the current one) which starts
        // before the current group ends and ends after the current group starts

        for other_group in group_ids.iter() {
            if other_group <= current_group {
                continue;
            }

            let (current_offset, current_size) = group_footprints[current_group];
            let (other_offset, other_size) = group_footprints[other_group];

            let current_start = current_offset;
            let current_end = current_offset + 0x4 * current_size;
            let other_start = other_offset;
            let other_end = other_offset + 0x4 * other_size;

            if current_end >= other_start && other_end >= current_start {
                // Compute the overlap
                let overlap = (current_end - other_start) / 4;
                let new_size = current_size - overlap;

                if DEBUG_PRINT_MAP_LOADING {
                    println!(
                        "Found and fixed overlap of {} at {:>3}->{:>3}  ${:07X}-${:07X}  ${:07X}-${:07X} Now group {} resized from {} to {}",
                        overlap, current_group, other_group, current_start, current_end, other_start, other_end, current_group, current_size,
                        new_size
                    );
                }

                // Update the current count
                group_footprints.insert(*current_group, (current_offset, new_size));
            }
        }
    }

    if DEBUG_PRINT_MAP_LOADING {
        println!();
        println!();
        println!();
        println!();
    }

    // Compose the groups
    let mut groups = Vec::new();
    let last_id = group_ids[group_ids.len() - 1];

    for group_id in 0..last_id + 1 {
        let group_reference = table_offset + 0x4 * group_id;

        let table = if let Some((group_offset, group_size)) = group_footprints.get(&group_id) {
            RomTable {
                offset: *group_offset,
                length: *group_size,
                references: vec![group_reference],
            }
        } else {
            RomTable {
                offset: 0, // could be anything
                length: 0,
                references: vec![group_reference],
            }
        };

        groups.push(table);
    }

    // Find the references to the table
    let table_references = rom.data.find_references(table_offset, 4);

    Ok(MapGroups {
        table: RomTable {
            offset: table_offset,
            length: last_id + 1,
            references: table_references,
        },
        groups,
    })
}

/// Find the offset to the map groups table in the ROM.
fn find_map_groups_reference_in_rom(rom: &Rom) -> Result<usize, RomIoError> {
    let bytes = [
        0x00, 0x04, 0x09, 0x04, 0x03, 0x4A, 0x80, 0x0B, 0x80, 0x18, 0x00, 0x68, 0x89, 0x0B, 0x09,
        0x18, 0x08, 0x68, 0x70, 0x47,
    ];
    let references = rom.data.find_bytes(&bytes);
    if references.is_empty() {
        println!("[ERROR] Could not find the string in the ROM");
        return Err(RomIoError::ReadingInvalidPointer(0, 0));
    }

    let reference = references[0] + 20;
    let offset = rom.data.read_offset(reference)?;

    return Ok(offset);
}

/// A problem that may be encountered when loading a map header from a reference.
///
/// `MapHeaderProblem::Ok` means that the map header is valid.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum MapHeaderProblem {
    /// The map header is valid.
    Ok = 0,

    /// The space where the pointer should be is free.
    FreePointerSpace,
    /// The pointer to the map is invalid.
    InvalidPointer,
    /// The pointer to the map is NULL.
    NullPointer,
    /// The offset is not aligned to 4.
    MisalignedOffset,

    /// Error reading the map header.
    ErrorReadingHeader,

    /// Header contains some invalid offset
    InvalidOffsetInHeader,
    /// All offsets are null.
    AllOffsetsNull,

    /// The music value is invalid.
    InvalidMusicValue,
    /// Cave value is not a boolean.
    InvalidCaveValue,
    /// Map type is outside of range.
    InvalidMapType,
    /// Battle type is outside of range.
    InvalidBattleType,

    /// The events are invalid.
    InvalidEvents,
    /// The connections are invalid.
    InvalidConnections,
    /// The scripts are invalid.
    InvalidScripts,

    /// The layout offset is NULL but the id is not 0
    NullLayoutIdMismatch,
    /// The layout offset exists, but it is misaligned.
    MisalignedLayoutOffset,
    /// The layout is invalid
    InvalidLayout,
    /// The layout could not be read
    ErrorReadingLayout,
    /// The layout has been freed
    FreedLayout,
}

impl std::fmt::Display for MapHeaderProblem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;
        write!(
            f,
            "{}",
            match self {
                // Problem while reading the offset
                Self::FreePointerSpace => "f".red(),
                Self::InvalidPointer => "i".red(),
                Self::NullPointer => "0".red(),
                Self::MisalignedOffset => "2".red(),

                // Problem while reading the header
                Self::ErrorReadingHeader => "e".red().on_bright_yellow(),

                // Problem while reading the values in the header
                Self::AllOffsetsNull => "a".black().on_red(),
                Self::InvalidOffsetInHeader => "o".black().on_red(),
                Self::InvalidMusicValue => "m".on_bright_yellow(),
                Self::InvalidCaveValue => "c".on_bright_yellow(),
                Self::InvalidMapType => "t".on_bright_yellow(),
                Self::InvalidBattleType => "b".on_bright_yellow(),

                // Problem with a sub-struct
                Self::InvalidEvents => "E".on_bright_yellow(),
                Self::InvalidConnections => "C".on_bright_yellow(),
                Self::InvalidScripts => "S".on_bright_yellow(),

                // Problem while reading the layout
                Self::NullLayoutIdMismatch => "N".yellow(),
                Self::MisalignedLayoutOffset => "3".yellow(),
                Self::InvalidLayout => "I".yellow(),
                Self::ErrorReadingLayout => "E".yellow(),
                Self::FreedLayout => "F".yellow(),

                Self::Ok => "✔".bright_green(),
            }
        )
    }
}

impl MapHeaderProblem {
    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Ok)
    }
}

/// Get a problem with a map header reference, and if the reference is valid,
/// get an eventual problem with the header itself.
pub fn get_map_header_pointer_problem(rom: &Rom, header_pointer: Offset) -> MapHeaderProblem {
    // Try to read the pointer
    match rom.data.read::<RomPointer>(header_pointer) {
        Ok(RomPointer::Null) => MapHeaderProblem::NullPointer,
        Ok(RomPointer::Invalid(_)) => MapHeaderProblem::InvalidPointer,
        Ok(RomPointer::Valid(offset, _)) | Ok(RomPointer::NoData(offset)) => {
            match rom.data.read::<MapHeader>(offset) {
                Ok(header) => get_map_header_problem(rom, header),
                Err(RomIoError::Misaligned(_, _)) => MapHeaderProblem::MisalignedOffset,
                Err(_) => MapHeaderProblem::ErrorReadingHeader,
            }
        }
        Err(_) => MapHeaderProblem::FreePointerSpace,
    }
}

/// Returns whether the given MapHeader is valid, otherwise returns
/// one of the problems.
fn get_map_header_problem(rom: &Rom, header: MapHeader) -> MapHeaderProblem {
    // All pointers must be valid
    if !(header.connections.is_valid()
        && header.layout.is_valid()
        && header.events.is_valid()
        && header.scripts.is_valid())
    {
        return MapHeaderProblem::InvalidOffsetInHeader;
    }
    // There must be at least a non-null offset
    if header.connections.is_null()
        && header.layout.is_null()
        && header.events.is_null()
        && header.scripts.is_null()
    {
        return MapHeaderProblem::AllOffsetsNull;
    }
    // All other values must make sense
    if !(header.music & 0x7FFF == 0x7FFF || header.music < 2000) {
        return MapHeaderProblem::InvalidMusicValue;
    }
    // Cave must be a boolean
    // TODO See how much this influences the results
    if header.cave > 2 {
        return MapHeaderProblem::InvalidCaveValue;
    }
    // Map type must be within range
    if header.map_type > 9 {
        return MapHeaderProblem::InvalidMapType;
    }
    // Battle type must be within range
    if header.battle_type > 8 {
        return MapHeaderProblem::InvalidBattleType;
    }

    //  Either the layout is NULL and the layout_id is 0
    if header.layout.is_null() && header.layout_id != 0 {
        return MapHeaderProblem::NullLayoutIdMismatch;
    }
    // If the layout offset is not null (and valid)
    else if let Some(layout_offset) = header.layout.offset() {
        // The offset to the layout is not aligned to 4
        if layout_offset % 4 != 0 {
            return MapHeaderProblem::MisalignedLayoutOffset;
        }

        //  The used map layout must be valid
        match rom.data.read::<MapLayout>(layout_offset) {
            Ok(layout) => {
                if !layout.is_valid() {
                    if rom
                        .data
                        .read_slice(layout_offset, MapLayout::get_size(&rom.data))
                        .unwrap_or(&[])
                        .iter()
                        .all(|x| *x == 0xFF)
                    {
                        return MapHeaderProblem::FreedLayout;
                    } else {
                        return MapHeaderProblem::InvalidLayout;
                    }
                }
            }
            // This case is extremely unlikely
            Err(_) => return MapHeaderProblem::ErrorReadingLayout,
        };
    }

    // If there are events, they must be valid
    if let Some(events) = header.events.offset() {
        if !are_events_valid(rom, events) {
            return MapHeaderProblem::InvalidEvents;
        }
    }

    // If there are connections, they must be valid
    if let Some(connections) = header.connections.offset() {
        // Read the connections number
        let connections_number = rom.data.read::<i32>(connections).ok();
        if let Some(connections_number) = connections_number {
            if connections_number > 128 || connections_number < 0 {
                return MapHeaderProblem::InvalidConnections;
            }
        }

        let connections_pointer = rom.data.read::<RomPointer>(connections + 4).ok();
        if !connections_pointer.map(|x| x.is_valid()).unwrap_or(false) {
            return MapHeaderProblem::InvalidConnections;
        }
    }

    // If there are scripts, they must be valid
    if let Some(scripts) = header.scripts.offset() {
        if !are_scripts_valid(rom, scripts) {
            return MapHeaderProblem::InvalidScripts;
        }
    }

    MapHeaderProblem::Ok
}

/// Check if the MapEvents struct starting at the given offset is valid.
fn are_events_valid(rom: &Rom, offset: Offset) -> bool {
    // Make sure all pointers in the map events struct are valid
    let object_events = rom.data.read::<RomPointer>(offset + 4).ok();
    let warps = rom.data.read::<RomPointer>(offset + 8).ok();
    let coord_events = rom.data.read::<RomPointer>(offset + 12).ok();
    let bg_events = rom.data.read::<RomPointer>(offset + 16).ok();

    object_events.map(|x| x.is_valid()).unwrap_or(false)
        && warps.map(|x| x.is_valid()).unwrap_or(false)
        && coord_events.map(|x| x.is_valid()).unwrap_or(false)
        && bg_events.map(|x| x.is_valid()).unwrap_or(false)
}

/// Check if the map scripts are valid.
fn are_scripts_valid(rom: &Rom, offset: Offset) -> bool {
    // Read a byte, followed by a pointer until the byte is 0.
    let mut read_size = 0;

    while read_size < 0x100 {
        let kind = match rom.data.read_byte(offset + read_size) {
            Ok(byte) => byte,
            Err(_) => return false,
        };
        read_size += 1;

        match kind {
            0 => break,
            1..=ON_RETURN_TO_FIELD => {}
            _ => {
                return false;
            }
        }

        match rom.data.read_word(offset + read_size) {
            Ok(ptr) => {
                if !rom.data.is_pointer_valid(ptr) {
                    return false;
                }
            }
            Err(_) => return false,
        };
        read_size += 4;
    }

    true
}
