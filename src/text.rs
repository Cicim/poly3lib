use gba_types::GBAIOError;

use crate::rom::Rom;

const ___: &'static str = "�";
const LV_: &'static str = "ᴸᵛ";
const PK_: &'static str = "ᴾₖ";
const MN_: &'static str = "ᴹₙ";
const PO_: &'static str = "ᴾₒ";
const KE_: &'static str = "ᴷₑ";
const BL1: &'static str = "BL";
const BL2: &'static str = "O";
const BL3: &'static str = "CK";
const RE_: &'static str = "ʳᵉ";

/// Text encoding for the international version of the game.
const INT_TEXT_ENCODING: [&'static str; 256] = [
    /*$00*/ " ", "À", "Á", "Â", "Ç", "È", "É", "Ê", "Ë", "Ì", ___, "Î", "Ï", "Ò", "Ó", "Ô",
    /*$10*/ "Œ", "Ù", "Ú", "Û", "Ñ", "ß", "à", "á", ___, "ç", "è", "é", "ê", "ë", "ì", ___,
    /*$20*/ "î", "ï", "ò", "ó", "ô", "œ", "ù", "ú", "û", "ñ", "º", "ª", "ᵉʳ", "&", "+", ___,
    /*$30*/ ___, ___, ___, ___, LV_, "=", ";", ___, ___, ___, ___, ___, ___, ___, ___, ___,
    /*$40*/ ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___,
    /*$50*/ "▯", "¿", "¡", PK_, MN_, PO_, KE_, BL1, BL2, BL3, "Í", "%", "(", ")", ___, ___,
    /*$60*/ ___, ___, ___, ___, ___, ___, ___, ___, "â", ___, ___, ___, ___, ___, ___, "í",
    /*$70*/ ___, ___, ___, ___, ___, ___, ___, ___, ___, "⬆", "⬇", "⬅", "➡", "*", "*", "*",
    /*$80*/ "*", "*", "*", "*", "ᵉ", "<", ">", ___, ___, ___, ___, ___, ___, ___, ___, ___,
    /*$90*/ ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___,
    /*$A0*/ RE_, "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "!", "?", ".", "-", "・",
    /*$B0*/ "…", "“", "”", "‘", "’", "♂", "♀", "$", ",", "×", "/", "A", "B", "C", "D", "E",
    /*$C0*/ "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U",
    /*$D0*/ "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
    /*$E0*/ "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "▶",
    /*$F0*/ ":", "Ä", "Ö", "Ü", "ä", "ö", "ü", ___, ___, ___, ___, ___, ___, ___, ___, ___,
];

pub struct Text {
    tokens: Vec<TextToken>,
    pub length: usize,
}

pub enum TextToken {
    Symbol(u8),
    NewLine(u8),
    Placeholder(u8, u8),
    Control(u8),
}

pub enum TextError {
    InvalidOffset,
}

impl Text {
    pub fn to_string(&self) -> String {
        let mut res = String::new();

        for token in self.tokens.iter() {
            match token {
                TextToken::Symbol(byte) => {
                    res.push_str(INT_TEXT_ENCODING[*byte as usize]);
                }
                TextToken::Control(byte) => {
                    res.push_str(format!("${:02X}", byte).as_str());
                }
                _ => (),
            }
        }

        res
    }

    pub fn split_by_newline(self) -> Vec<Vec<TextToken>> {
        let mut lines = vec![];

        let mut current_line: Vec<TextToken> = vec![];
        // Every time you encounter a newline token, keep it in the first string
        // then start a new one.
        for token in self.tokens {
            let is_newline = matches!(token, TextToken::NewLine(_));

            current_line.push(token);
            if is_newline {
                lines.push(current_line);
                current_line = vec![];
            }
        }

        // Add the last line if not empty
        if !current_line.is_empty() {
            lines.push(current_line);
        }

        lines
    }

    pub fn debug_format(line: Vec<TextToken>) -> String {
        let mut res = String::new();

        for token in line.iter() {
            match token {
                TextToken::Symbol(byte) => {
                    res.push_str(INT_TEXT_ENCODING[*byte as usize]);
                }
                TextToken::Control(byte) => {
                    res.push_str(format!("${:02X}", byte).as_str());
                }
                // TODO Consider other types of newlines
                TextToken::NewLine(b) => res.push_str(match b {
                    0xFB => "\\n",
                    0xFE => "\\p",
                    _ => "\\?",
                }),
                TextToken::Placeholder(_, code) => {
                    let code = match *code {
                        0x0 => "UNKNOWN",
                        0x1 => "PLAYER",
                        0x2 => "STRING_VAR_1",
                        0x3 => "STRING_VAR_2",
                        0x4 => "STRING_VAR_3",
                        0x5 => "KUN",
                        0x6 => "RIVAL",
                        0x7 => "VERSION",
                        0x8 => "MAGMA",
                        0x9 => "AQUA",
                        0xA => "MAXIE",
                        0xB => "ARCHIE",
                        0xC => "GROUDON",
                        0xD => "KYOGRE",
                        _ => "???",
                    };

                    res.push_str(format!("{{{}}}", code).as_str());
                }
            }
        }

        res
    }
}

// TODO Decide best text lenght limit for safety reasons
const MAX_TEXT_LENGTH: usize = 0x400;

impl Rom {
    /// Reads a [`Text`] from the ROM.
    pub fn read_text(&self, offset: usize) -> Result<Text, TextError> {
        let mut tokens = vec![];
        let mut length = 0;

        while length < MAX_TEXT_LENGTH {
            let byte: u8 = self
                .read(offset + length)
                .map_err(|_| TextError::InvalidOffset)?;
            length += 1;

            tokens.push(match byte {
                0x00..=0xF7 => TextToken::Symbol(byte),
                0xFB | 0xFE => TextToken::NewLine(byte),
                0xFD => {
                    // Read the next byte
                    let code: u8 = self
                        .read(offset + length)
                        .map_err(|_| TextError::InvalidOffset)?;
                    length += 1;

                    TextToken::Placeholder(byte, code)
                }
                0xF8..=0xFD => TextToken::Control(byte),
                0xFF => break,
            });
        }

        Ok(Text { tokens, length })
    }

    /// Clears the content of the text at the given offset.
    ///
    /// It is not its responsibility to make sure this string
    /// is not used by anyone.
    pub fn clear_text(&mut self, offset: usize) -> Result<(), GBAIOError> {
        let end = self.find_byte_after(offset, 0xFF);
        if let Some(end) = end {
            if end - offset > MAX_TEXT_LENGTH {
                return Err(GBAIOError::Unknown(
                    "Trying to delete a text that is too long",
                ));
            }

            self.clear(offset, end - offset)?;
        }

        Ok(())
    }
}
