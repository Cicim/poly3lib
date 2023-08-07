use gba_types::GBAIOError;
use thiserror::Error;

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

/// Icons representing GBA buttons. (after a 0xF8 byte)
#[repr(u8)]
#[derive(Debug)]
pub enum TextKeyPadIcon {
    AButton = 0x00,
    BButton = 0x01,
    LButton = 0x02,
    RButton = 0x03,
    StartButton = 0x04,
    SelectButton = 0x05,
    DpadUp = 0x06,
    DpadDown = 0x07,
    DpadLeft = 0x08,
    DpadRight = 0x09,
    DpadUpdown = 0x0A,
    DpadLeftright = 0x0B,
    DpadNone = 0x0C,
}

impl TryFrom<u8> for TextKeyPadIcon {
    type Error = TextError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use TextKeyPadIcon::*;
        Ok(match value {
            0x00 => AButton,
            0x01 => BButton,
            0x02 => LButton,
            0x03 => RButton,
            0x04 => StartButton,
            0x05 => SelectButton,
            0x06 => DpadUp,
            0x07 => DpadDown,
            0x08 => DpadLeft,
            0x09 => DpadRight,
            0x0A => DpadUpdown,
            0x0B => DpadLeftright,
            0x0C => DpadNone,
            _ => return Err(TextError::InvalidKeyPadIcon(value)),
        })
    }
}

// Extra symbol (after a 0xF9 byte)
#[repr(u8)]
#[derive(Debug)]
pub enum TextExtraSymbol {
    UpArrow = 0x00,
    DownArrow = 0x01,
    LeftArrow = 0x02,
    RightArrow = 0x03,
    Plus = 0x04,
    Lv = 0x05,
    Pp = 0x06,
    Id = 0x07,
    No = 0x08,
    Underscore = 0x09,
    Circled1 = 0x0A,
    Circled2 = 0x0B,
    Circled3 = 0x0C,
    Circled4 = 0x0D,
    Circled5 = 0x0E,
    Circled6 = 0x0F,
    Circled7 = 0x10,
    Circled8 = 0x11,
    Circled9 = 0x12,
    LeftParenSmall = 0x13,
    RightParenSmall = 0x14,
    Bullseye = 0x15,
    Triangle = 0x16,
    CrossX = 0x17,
}

impl TryFrom<u8> for TextExtraSymbol {
    type Error = TextError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use TextExtraSymbol::*;
        Ok(match value {
            0x00 => UpArrow,
            0x01 => DownArrow,
            0x02 => LeftArrow,
            0x03 => RightArrow,
            0x04 => Plus,
            0x05 => Lv,
            0x06 => Pp,
            0x07 => Id,
            0x08 => No,
            0x09 => Underscore,
            0x0A => Circled1,
            0x0B => Circled2,
            0x0C => Circled3,
            0x0D => Circled4,
            0x0E => Circled5,
            0x0F => Circled6,
            0x10 => Circled7,
            0x11 => Circled8,
            0x12 => Circled9,
            0x13 => LeftParenSmall,
            0x14 => RightParenSmall,
            0x15 => Bullseye,
            0x16 => Triangle,
            0x17 => CrossX,
            _ => return Err(TextError::InvalidExtraSymbol(value)),
        })
    }
}

/// Placeholder strings (after a 0xFC byte)
#[repr(u8)]
#[derive(Debug)]
pub enum TextPlaceHolder {
    Unknown = 0x0,
    Player = 0x1,
    StringVar1 = 0x2,
    StringVar2 = 0x3,
    StringVar3 = 0x4,
    Kun = 0x5,
    Rival = 0x6,
    Version = 0x7,
    Magma = 0x8,
    Aqua = 0x9,
    Maxie = 0xA,
    Archie = 0xB,
    Groudon = 0xC,
    Kyogre = 0xD,
}

impl TryFrom<u8> for TextPlaceHolder {
    type Error = TextError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use TextPlaceHolder::*;
        Ok(match value {
            0x00 => Unknown,
            0x01 => Player,
            0x02 => StringVar1,
            0x03 => StringVar2,
            0x04 => StringVar3,
            0x05 => Kun,
            0x06 => Rival,
            0x07 => Version,
            0x08 => Magma,
            0x09 => Aqua,
            0x0A => Maxie,
            0x0B => Archie,
            0x0C => Groudon,
            0x0D => Kyogre,
            _ => return Err(TextError::InvalidPlaceHolder(value)),
        })
    }
}

/// Control Codes (after a 0xFD byte)
#[repr(u8)]
#[derive(Debug)]
pub enum TextExtCtrlCode {
    Highlight = 0x02,
    Shadow = 0x03,
    ColorHighlightShadow = 0x04,
    Palette = 0x05,
    Font = 0x06,
    ResetFont = 0x07,
    Pause = 0x08,
    PauseUntilPress = 0x09,
    WaitSe = 0x0A,
    PlayBgm = 0x0B,
    Escape = 0x0C,
    ShiftRight = 0x0D,
    ShiftDown = 0x0E,
    FillWindow = 0x0F,
    PlaySe = 0x10,
    Clear = 0x11,
    Skip = 0x12,
    ClearTo = 0x13,
    MinLetterSpacing = 0x14,
    Jpn = 0x15,
    Eng = 0x16,
    PauseMusic = 0x17,
    ResumeMusic = 0x18,
}

impl TryFrom<u8> for TextExtCtrlCode {
    type Error = TextError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use TextExtCtrlCode::*;
        Ok(match value {
            0x02 => Highlight,
            0x03 => Shadow,
            0x04 => ColorHighlightShadow,
            0x05 => Palette,
            0x06 => Font,
            0x07 => ResetFont,
            0x08 => Pause,
            0x09 => PauseUntilPress,
            0x0A => WaitSe,
            0x0B => PlayBgm,
            0x0C => Escape,
            0x0D => ShiftRight,
            0x0E => ShiftDown,
            0x0F => FillWindow,
            0x10 => PlaySe,
            0x11 => Clear,
            0x12 => Skip,
            0x13 => ClearTo,
            0x14 => MinLetterSpacing,
            0x15 => Jpn,
            0x16 => Eng,
            0x17 => PauseMusic,
            0x18 => ResumeMusic,
            _ => return Err(TextError::InvalidExtCtrlCode(value)),
        })
    }
}

/// Text colors -- they represent which color to apply
/// from a palette of 16 colors.
#[repr(u8)]
#[derive(Debug)]
pub enum TextColor {
    Transparent = 0x0,
    White = 0x1,
    DarkGray = 0x2,
    LightGray = 0x3,
    Red = 0x4,
    LightRed = 0x5,
    Green = 0x6,
    LightGreen = 0x7,
    Blue = 0x8,
    LightBlue = 0x9,
    Color1 = 0xA, // Usually white
    Color2 = 0xB, // Usually white w/ tinge of green
    Color3 = 0xC, // Usually white
    Color4 = 0xD, // Usually aquamarine
    Color5 = 0xE, // Usually blue-green
    Color6 = 0xF, // Usually cerulean
}

impl TryFrom<u8> for TextColor {
    type Error = TextError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use TextColor::*;
        Ok(match value {
            0x00 => Transparent,
            0x01 => White,
            0x02 => DarkGray,
            0x03 => LightGray,
            0x04 => Red,
            0x05 => LightRed,
            0x06 => Green,
            0x07 => LightGreen,
            0x08 => Blue,
            0x09 => LightBlue,
            0x0A => Color1,
            0x0B => Color2,
            0x0C => Color3,
            0x0D => Color4,
            0x0E => Color5,
            0x0F => Color6,
            _ => return Err(TextError::InvalidColor(value)),
        })
    }
}

/// A Token in a text string.
#[repr(u8)]
pub enum TextToken {
    /// A normal symbol (in the range 0x00-0xF6)
    Symbol(u8),

    /// `0xF7` Dynamic text got from elsewhere
    Dynamic,
    /// `0xF8` Keypad icon. The next byte is the icon index.
    KeyPadIcon(TextKeyPadIcon),
    /// `0xF9` Extra symbol. The next byte is the symbol index.
    ExtraSymbol(TextExtraSymbol),
    /// `0xFA` Waits for button press and scrolls dialog
    PromptScroll,
    /// `0xFB` Waits for button press and clears dialog
    PromptClear,
    /// `0xFC` A code for specifying placeholder text. The next byte is the placeholder index.
    PlaceHolder(TextPlaceHolder),
    /// `0xFD` Extended control code. The next byte is the control code.
    ExtCtrlCode(TextExtCtrlCode),
    /// `0xFD`0x01] Color. The next byte is the color index.
    Color(TextColor),
    /// `0xFE` Newline
    NewLine,

    /// `0xFF` Not read
    EOS,
}

impl TextToken {
    fn is_newline(&self) -> bool {
        use TextToken::*;
        matches!(self, NewLine | PromptScroll | PromptClear)
    }

    fn format_debug(&self) -> String {
        use TextToken::*;
        match self {
            Symbol(s) => INT_TEXT_ENCODING[*s as usize],
            NewLine => "\\n",
            PromptScroll => "\\p",
            PromptClear => "\\l",
            Dynamic => "{dyn}",
            PlaceHolder(p) => return format!("{{{:?}}}", p),
            KeyPadIcon(i) => return format!("{{key:{:?}}}", i),
            ExtraSymbol(s) => return format!("{{sym:{:?}}}", s),
            ExtCtrlCode(c) => return format!("{{ctrl:{:?}}}", c),
            Color(c) => return format!("{{color:{:?}}}", c),
            EOS => "$",
        }
        .into()
    }

    // TODO You can do more interesting things in display mode
    fn format_display(&self) -> String {
        use TextToken::*;
        match self {
            Symbol(s) => INT_TEXT_ENCODING[*s as usize],
            NewLine => "\n".into(),
            PromptScroll => "\n".into(),
            PromptClear => "\n".into(),
            Dynamic => "{dyn}".into(),
            // In Display mode, placeholders blend in with the rest of the text
            PlaceHolder(p) => return format!("{:?}", p),
            KeyPadIcon(i) => return format!("{{key:{:?}}}", i),
            ExtraSymbol(s) => return format!("{{sym:{:?}}}", s),
            ExtCtrlCode(c) => return format!("{{ctrl:{:?}}}", c),
            Color(c) => return format!("{{color:{:?}}}", c),
            EOS => "$",
        }
        .into()
    }
}

#[derive(Error, Debug)]
pub enum TextError {
    #[error("Invalid offset {0}")]
    InvalidOffset(usize),
    #[error("Invalid keypad icon symbol {0}")]
    InvalidKeyPadIcon(u8),
    #[error("Invalid extra symbol {0}")]
    InvalidExtraSymbol(u8),
    #[error("Invalid placeholder {0}")]
    InvalidPlaceHolder(u8),
    #[error("Invalid extended control code {0}")]
    InvalidExtCtrlCode(u8),
    #[error("Invalid color {0}")]
    InvalidColor(u8),
}

impl Text {
    /// Converts text to a readable string.
    pub fn to_string(&self) -> String {
        self.tokens.iter().map(TextToken::format_display).collect()
    }

    /// Takes a [`Text`] object and returns a vector of substrings of that text
    /// (represented as [`Vec<TextToken>`]) split by newlines. The newline character
    /// is kept as the last character in the substring.
    pub fn split_by_newline(self) -> Vec<Vec<TextToken>> {
        let mut lines = vec![];

        let mut current_line: Vec<TextToken> = vec![];
        // Every time you encounter a newline token, keep it in the first string
        // then start a new one.
        for token in self.tokens {
            let is_newline = token.is_newline();
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

    /// Formats a vector of [`TextToken`]s into a string for debugging purposes.
    pub fn debug_format(line: Vec<TextToken>) -> String {
        line.iter().map(TextToken::format_debug).collect()
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
            let codepoint: u8 = self
                .read(offset + length)
                .map_err(|_| TextError::InvalidOffset(offset + length))?;
            length += 1;

            tokens.push(match codepoint {
                // Normal characters
                0x00..=0xF6 => TextToken::Symbol(codepoint),
                // Special characters
                0xF7 => TextToken::Dynamic,
                0xFA => TextToken::PromptScroll,
                0xFB => TextToken::PromptClear,
                0xFE => TextToken::NewLine,
                // End of string
                0xFF => break,

                // Special characters requiring extra bytes
                _ => {
                    // All other codes need at least an extra byte
                    let specifier: u8 = self
                        .read(offset + length)
                        .map_err(|_| TextError::InvalidOffset(offset + length))?;
                    length += 1;

                    match codepoint {
                        // Extra characters
                        0xF8 => TextToken::KeyPadIcon(TextKeyPadIcon::try_from(specifier)?),
                        0xF9 => TextToken::ExtraSymbol(TextExtraSymbol::try_from(specifier)?),
                        0xFC => TextToken::PlaceHolder(TextPlaceHolder::try_from(specifier)?),
                        0xFD => {
                            if specifier == 1 {
                                let color: u8 = self
                                    .read(offset + length)
                                    .map_err(|_| TextError::InvalidOffset(offset + length))?;
                                length += 1;
                                TextToken::Color(TextColor::try_from(color)?)
                            } else {
                                TextToken::ExtCtrlCode(TextExtCtrlCode::try_from(specifier)?)
                            }
                        }
                        _ => unreachable!("everything should be covered"),
                    }
                }
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
