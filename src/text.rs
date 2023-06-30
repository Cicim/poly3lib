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

pub struct Text(Vec<TextToken>);

pub enum TextToken {
    Symbol(u8),
    Control(u8),
}

pub enum TextError {
    InvalidOffset,
}

impl Text {
    pub fn to_string(&self) -> String {
        let mut res = String::new();

        for token in &self.0 {
            match token {
                TextToken::Symbol(byte) => {
                    res.push_str(INT_TEXT_ENCODING[*byte as usize]);
                }
                TextToken::Control(byte) => {
                    res.push_str(format!("${:02X}", byte).as_str());
                }
            }
        }

        res
    }
}

impl Rom {
    /// Reads a [`Text`] from the ROM.
    pub fn read_text(&self, offset: usize) -> Result<Text, TextError> {
        let mut res = Text(vec![]);
        let mut i = 0;

        // TODO Is 256 bytes enough?
        while i < 256 {
            let byte: u8 = self
                .read(offset + i)
                .map_err(|_| TextError::InvalidOffset)?;

            res.0.push(match byte {
                0x00..=0xF7 => TextToken::Symbol(byte),
                0xF8..=0xFE => TextToken::Control(byte),
                0xFF => break,
            });

            i += 1;
        }

        Ok(res)
    }
}
