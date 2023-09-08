use thiserror::Error;

use crate::{Offset, RomData, RomIoError};

use super::RomReadableType;

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

/// Encoded text from the ROM.
#[derive(Debug)]
pub struct RomText {
    /// List of tokens in the text.
    tokens: Vec<TextToken>,
    /// Length of the text in bytes.
    pub size: usize,
}

/// Icons representing GBA buttons. (after a 0xF8 byte)
#[repr(u8)]
#[derive(Debug)]
pub enum TextKeyPadIcon {
    A = 0x00,
    B = 0x01,
    L = 0x02,
    R = 0x03,
    Start = 0x04,
    Select = 0x05,

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
        use TextKeyPadIcon as I;
        Ok(match value {
            0x00 => I::A,
            0x01 => I::B,
            0x02 => I::L,
            0x03 => I::R,
            0x04 => I::Start,
            0x05 => I::Select,
            0x06 => I::DpadUp,
            0x07 => I::DpadDown,
            0x08 => I::DpadLeft,
            0x09 => I::DpadRight,
            0x0A => I::DpadUpdown,
            0x0B => I::DpadLeftright,
            0x0C => I::DpadNone,
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
        use TextExtraSymbol as S;
        Ok(match value {
            0x00 => S::UpArrow,
            0x01 => S::DownArrow,
            0x02 => S::LeftArrow,
            0x03 => S::RightArrow,
            0x04 => S::Plus,
            0x05 => S::Lv,
            0x06 => S::Pp,
            0x07 => S::Id,
            0x08 => S::No,
            0x09 => S::Underscore,
            0x0A => S::Circled1,
            0x0B => S::Circled2,
            0x0C => S::Circled3,
            0x0D => S::Circled4,
            0x0E => S::Circled5,
            0x0F => S::Circled6,
            0x10 => S::Circled7,
            0x11 => S::Circled8,
            0x12 => S::Circled9,
            0x13 => S::LeftParenSmall,
            0x14 => S::RightParenSmall,
            0x15 => S::Bullseye,
            0x16 => S::Triangle,
            0x17 => S::CrossX,
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
        use TextPlaceHolder as P;
        Ok(match value {
            0x00 => P::Unknown,
            0x01 => P::Player,
            0x02 => P::StringVar1,
            0x03 => P::StringVar2,
            0x04 => P::StringVar3,
            0x05 => P::Kun,
            0x06 => P::Rival,
            0x07 => P::Version,
            0x08 => P::Magma,
            0x09 => P::Aqua,
            0x0A => P::Maxie,
            0x0B => P::Archie,
            0x0C => P::Groudon,
            0x0D => P::Kyogre,
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
        use TextExtCtrlCode as C;
        Ok(match value {
            0x02 => C::Highlight,
            0x03 => C::Shadow,
            0x04 => C::ColorHighlightShadow,
            0x05 => C::Palette,
            0x06 => C::Font,
            0x07 => C::ResetFont,
            0x08 => C::Pause,
            0x09 => C::PauseUntilPress,
            0x0A => C::WaitSe,
            0x0B => C::PlayBgm,
            0x0C => C::Escape,
            0x0D => C::ShiftRight,
            0x0E => C::ShiftDown,
            0x0F => C::FillWindow,
            0x10 => C::PlaySe,
            0x11 => C::Clear,
            0x12 => C::Skip,
            0x13 => C::ClearTo,
            0x14 => C::MinLetterSpacing,
            0x15 => C::Jpn,
            0x16 => C::Eng,
            0x17 => C::PauseMusic,
            0x18 => C::ResumeMusic,
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
        use TextColor as C;
        Ok(match value {
            0x00 => C::Transparent,
            0x01 => C::White,
            0x02 => C::DarkGray,
            0x03 => C::LightGray,
            0x04 => C::Red,
            0x05 => C::LightRed,
            0x06 => C::Green,
            0x07 => C::LightGreen,
            0x08 => C::Blue,
            0x09 => C::LightBlue,
            0x0A => C::Color1,
            0x0B => C::Color2,
            0x0C => C::Color3,
            0x0D => C::Color4,
            0x0E => C::Color5,
            0x0F => C::Color6,
            _ => return Err(TextError::InvalidColor(value)),
        })
    }
}

/// A Token in a text string.
#[repr(u8)]
#[derive(Debug)]
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

#[derive(Error, Debug, PartialEq, Eq)]
pub enum TextError {
    #[error("Text is too long")]
    TooLong,

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

impl RomText {
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

impl RomReadableType for RomText {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        // Read the text token by token
        let mut tokens = vec![];
        // The length of the text in bytes
        let mut size = 0;

        while let Ok(codepoint) = rom.read_byte(offset + size) {
            size += 1;

            if size > MAX_TEXT_LENGTH {
                return Err(TextError::TooLong.into());
            }

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

                // Extra characters
                0xF8 => {
                    let code = rom.read_byte(offset + size)?;
                    size += 1;
                    TextToken::KeyPadIcon(TextKeyPadIcon::try_from(code)?)
                }
                0xF9 => {
                    let code = rom.read_byte(offset + size)?;
                    size += 1;
                    TextToken::ExtraSymbol(TextExtraSymbol::try_from(code)?)
                }
                0xFC => {
                    let code = rom.read_byte(offset + size)?;
                    size += 1;
                    TextToken::PlaceHolder(TextPlaceHolder::try_from(code)?)
                }

                // Control characters
                0xFD => {
                    let code = rom.read_byte(offset + size)?;
                    size += 1;
                    match code {
                        0x01 => {
                            let color = rom.read_byte(offset + size)?;
                            size += 1;
                            TextToken::Color(TextColor::try_from(color)?)
                        }
                        _ => TextToken::ExtCtrlCode(TextExtCtrlCode::try_from(code)?),
                    }
                }
            });
        }

        Ok(Self { tokens, size })
    }
}
