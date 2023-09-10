use colored::Colorize;
use thiserror::Error;

use rom_data::{types::RomText, Offset, RomIoError};

use crate::Rom;

use super::{ScriptCommand, ScriptReadInstruction};

// ANCHOR Resources
/// Script resource as read in the tree.
#[derive(Debug)]
pub struct ScriptResource {
    /// Offset of the resource.
    pub offset: Offset,
    /// Size in bytes.
    pub size: usize,
    /// Type and content.
    pub value: ScriptResourceContent,
}

/// Type and content of a script resource.
#[derive(Debug)]
pub enum ScriptResourceContent {
    /// Script with its instruction
    Script(Vec<ScriptInstruction>),
    /// Movement with its commands (ending in 0xFE)
    Movement(Vec<u8>),
    /// Encoded text tokens.
    Text(RomText),
    /// Products list (ending in 0x0000)
    Products(Vec<u16>),
}

/// Instruction in a script resource.
#[derive(Debug)]
pub struct ScriptInstruction {
    pub code: u8,
    pub args: Vec<ScriptArgument>,
}

/// Argument of a script instruction.
#[derive(Debug)]
pub enum ScriptArgument {
    Byte(u8),
    Half(u16),
    Word(u32),
    Flag(u16),
    Var(u16),
    ScriptOffset(Offset),
    MovementOffset(Offset),
    TextOffset(Offset),
    ProductsOffset(Offset),
}

// ANCHOR Tree
/// Type and offset of a resource, for marking it as read.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScriptResourceMarker {
    Script(Offset),
    Movement(Offset),
    Text(Offset),
    Products(Offset),
}

impl ScriptResourceMarker {
    /// Returns this marker's offset.
    pub fn offset(&self) -> Offset {
        match self {
            ScriptResourceMarker::Script(offset) => *offset,
            ScriptResourceMarker::Movement(offset) => *offset,
            ScriptResourceMarker::Text(offset) => *offset,
            ScriptResourceMarker::Products(offset) => *offset,
        }
    }

    /// Returns this marker's priority.
    pub fn priority(&self) -> u8 {
        match self {
            ScriptResourceMarker::Script(_) => 0,
            ScriptResourceMarker::Movement(_) => 1,
            ScriptResourceMarker::Text(_) => 2,
            ScriptResourceMarker::Products(_) => 3,
        }
    }
}

#[derive(Debug, Error)]
pub enum ScriptResourceReadingError {
    #[error("The scripts table was not loaded")]
    ScriptsTableNotLoaded,
    #[error("Invalid command code {0}")]
    InvalidCommandCode(u8),

    #[error("IO error: {0}")]
    IoError(#[from] RomIoError),
}

// ANCHOR Reading
impl ScriptResourceMarker {
    /// Reads a resource content starting from a marker, and an
    /// optional reference to a vector for storing referenced resources.
    pub fn read(
        self,
        rom: &Rom,
        refs: Option<&mut Vec<ScriptResourceMarker>>,
    ) -> Result<ScriptResource, ScriptResourceReadingError> {
        match self {
            ScriptResourceMarker::Script(offset) => read_script(rom, offset, refs),
            ScriptResourceMarker::Movement(offset) => read_movements(rom, offset, None),
            ScriptResourceMarker::Text(offset) => read_text(rom, offset, None),
            ScriptResourceMarker::Products(offset) => read_products(rom, offset, None),
        }
    }
}

/// Read a script resource, saving referenced resources if required.
fn read_script(
    rom: &Rom,
    offset: Offset,
    mut refs: Option<&mut Vec<ScriptResourceMarker>>,
) -> Result<ScriptResource, ScriptResourceReadingError> {
    let cmd_table = get_commands_table(rom)?;

    // Output vector of read instructions for this script.
    let mut instructions: Vec<ScriptInstruction> = Vec::new();
    // Current byte index in the binary.
    let mut index = 0;
    // Index to an instruction that contains a Word that may needs
    // to be converted into a Text if the next instruction is std.
    let mut maybe_text_index = None;

    macro_rules! read_offset {
        ($pointer:ident, $args:ident, $argty:ident, $marker:ident) => {
            // If it is valid
            if rom.data.is_pointer_valid($pointer) {
                // Push it as an offset
                let offset = ($pointer - 0x08_000_000) as Offset;
                $args.push(A::$argty(offset));

                // Add the reference too
                if let Some(ref mut vec) = refs {
                    vec.push(ScriptResourceMarker::$marker(offset));
                }
            } else {
                // Otherwise, push it as a word.
                $args.push(A::Word($pointer));
            }
        };
    }
    macro_rules! push {
        ($args:ident, byte) => {{
            let byte = rom.data.read_byte(offset + index)?;
            index += 1;
            $args.push(A::Byte(byte));
            byte
        }};
        ($args:ident, half) => {{
            let half = rom.data.read_halfword(offset + index)?;
            index += 2;
            $args.push(A::Half(half));
            half
        }};
        ($args:ident, word) => {{
            let word = rom.data.read_word(offset + index)?;
            index += 4;
            $args.push(A::Word(word));
            word
        }};
    }

    let mut finished = false;

    // Always employ a limit for reading scripts
    while !finished && index < 0x1000 {
        // Read the instruction code
        let code = rom.data.read_byte(offset + index)?;
        // Get the instruction for that code
        let cmd = cmd_table
            .get(code as usize)
            .ok_or(ScriptResourceReadingError::InvalidCommandCode(code))?;
        index += 1;

        // Read the arguments
        let mut args = Vec::new();
        for read_instruction in cmd.read.iter() {
            use ScriptArgument as A;
            use ScriptReadInstruction as R;

            match read_instruction {
                // Simple values
                R::Byte => {
                    push!(args, byte);
                }
                R::Half => {
                    push!(args, half);
                }
                R::Word => {
                    push!(args, word);
                }
                R::Flag => {
                    let flag = rom.data.read_halfword(offset + index)?;
                    // REVIEW Check if the flag is valid?
                    index += 2;
                    args.push(A::Flag(flag));
                }
                R::Var => {
                    let var = rom.data.read_halfword(offset + index)?;
                    // REVIEW Check if the var is valid?
                    index += 2;
                    args.push(A::Var(var));
                }
                // Complex values
                R::TrainerBattle => {
                    // Battle types
                    const TRAINER_BATTLE_SINGLE: u8 = 0;
                    const TRAINER_BATTLE_CONTINUE_SCRIPT_NO_MUSIC: u8 = 1;
                    const TRAINER_BATTLE_CONTINUE_SCRIPT: u8 = 2;
                    const TRAINER_BATTLE_SINGLE_NO_INTRO_TEXT: u8 = 3;
                    const TRAINER_BATTLE_DOUBLE: u8 = 4;
                    const TRAINER_BATTLE_REMATCH: u8 = 5;
                    const TRAINER_BATTLE_CONTINUE_SCRIPT_DOUBLE: u8 = 6;
                    const TRAINER_BATTLE_REMATCH_DOUBLE: u8 = 7;
                    const TRAINER_BATTLE_CONTINUE_SCRIPT_DOUBLE_NO_MUSIC: u8 = 8;
                    const TRAINER_BATTLE_EARLY_RIVAL: u8 = 9;

                    // Common start arguments
                    // sTrainerBattleMode,           TRAINER_PARAM_LOAD_VAL_8BIT
                    push!(args, byte);
                    // gTrainerBattleOpponent_A,     TRAINER_PARAM_LOAD_VAL_16BIT
                    push!(args, half);
                    // sTrainerObjectEventLocalId,   TRAINER_PARAM_LOAD_VAL_16BIT
                    push!(args, half);

                    // Match based on the type
                    match push!(args, byte) {
                        // sOrdinaryNoIntroBattleParams
                        TRAINER_BATTLE_SINGLE_NO_INTRO_TEXT => {
                            // sTrainerAIntroSpeech,         TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                            // sTrainerADefeatSpeech,        TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                        }
                        // sDoubleBattleParams
                        TRAINER_BATTLE_DOUBLE | TRAINER_BATTLE_REMATCH_DOUBLE => {
                            // sTrainerAIntroSpeech,         TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                            // sTrainerADefeatSpeech,        TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                            // sTrainerCannotBattleSpeech,   TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                        }
                        // sContinueScriptBattleParams
                        TRAINER_BATTLE_CONTINUE_SCRIPT
                        | TRAINER_BATTLE_CONTINUE_SCRIPT_NO_MUSIC => {
                            // sTrainerAIntroSpeech,         TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                            // sTrainerADefeatSpeech,        TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                            // sTrainerABattleScriptRetAddr, TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, ScriptOffset, Script);
                        }
                        // sContinueScriptDoubleBattleParams
                        TRAINER_BATTLE_CONTINUE_SCRIPT_DOUBLE
                        | TRAINER_BATTLE_CONTINUE_SCRIPT_DOUBLE_NO_MUSIC => {
                            // sTrainerAIntroSpeech,         TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                            // sTrainerADefeatSpeech,        TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                            // sTrainerCannotBattleSpeech,   TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                            // sTrainerABattleScriptRetAddr, TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, ScriptOffset, Script);
                        }
                        // sEarlyRivalBattleParams
                        TRAINER_BATTLE_EARLY_RIVAL => {
                            // sTrainerADefeatSpeech,        TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                            // sTrainerVictorySpeech,        TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                        }
                        // sOrdinaryBattleParams
                        TRAINER_BATTLE_SINGLE | TRAINER_BATTLE_REMATCH | _ => {
                            // sTrainerAIntroSpeech,         TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                            // sTrainerADefeatSpeech,        TRAINER_PARAM_LOAD_VAL_32BIT
                            let pointer = rom.data.read_word(offset + index)?;
                            index += 4;
                            read_offset!(pointer, args, TextOffset, Text);
                        }
                    }
                }
                // Offsets
                R::ScriptOffset => {
                    let pointer = rom.data.read_word(offset + index)?;
                    index += 4;
                    read_offset!(pointer, args, ScriptOffset, Script);
                }
                R::MovementOffset => {
                    let pointer = rom.data.read_word(offset + index)?;
                    index += 4;
                    read_offset!(pointer, args, MovementOffset, Movement);
                }
                R::TextOffset => {
                    let pointer = rom.data.read_word(offset + index)?;
                    index += 4;
                    read_offset!(pointer, args, TextOffset, Text);
                }
                R::ProductsOffset => {
                    let pointer = rom.data.read_word(offset + index)?;
                    index += 4;
                    read_offset!(pointer, args, ProductsOffset, Products);
                }

                // Offset if std. Read a word for now
                R::TextOffsetIfStd => {
                    let word = rom.data.read_word(offset + index)?;
                    index += 4;
                    args.push(A::Word(word));

                    // Save the index of this instruction
                    maybe_text_index = Some(instructions.len());
                }

                R::Std => {
                    // If the previous instruction was a text offset
                    if let Some(index) = maybe_text_index {
                        let old_args = &mut instructions[index].args;
                        // Replace it with a text offset
                        if let A::Word(pointer) = old_args.pop().unwrap() {
                            read_offset!(pointer, old_args, TextOffset, Text);
                        }
                        // And remove the index
                        maybe_text_index = None;
                    }
                }
                R::Stop => finished = true,
            }
        }

        instructions.push(ScriptInstruction { code, args });
    }

    Ok(ScriptResource {
        offset,
        size: index,
        value: ScriptResourceContent::Script(instructions),
    })
}
/// Read a list of movements from a given offset.
fn read_movements(
    rom: &Rom,
    offset: Offset,
    _refs: Option<&mut Vec<ScriptResourceMarker>>,
) -> Result<ScriptResource, ScriptResourceReadingError> {
    let mut bytes = Vec::new();
    let mut index = 0;

    while index < 0x1000 {
        let byte = rom.data.read_byte(offset + index)?;
        bytes.push(byte);
        index += 1;

        if byte == 0xFE {
            break;
        }
    }

    Ok(ScriptResource {
        offset,
        size: index,
        value: ScriptResourceContent::Movement(bytes),
    })
}
/// Read a text from the given offset.
fn read_text(
    rom: &Rom,
    offset: Offset,
    _refs: Option<&mut Vec<ScriptResourceMarker>>,
) -> Result<ScriptResource, ScriptResourceReadingError> {
    let text = rom.data.read::<RomText>(offset)?;

    Ok(ScriptResource {
        offset,
        size: text.byte_size(),
        value: ScriptResourceContent::Text(text),
    })
}
/// Read a products list from a given offset
fn read_products(
    rom: &Rom,
    offset: Offset,
    _refs: Option<&mut Vec<ScriptResourceMarker>>,
) -> Result<ScriptResource, ScriptResourceReadingError> {
    let mut products = Vec::new();
    let mut index = 0;

    while index < 0x1000 {
        let word = rom.data.read_halfword(offset + index)?;
        products.push(word);
        index += 2;

        if word == 0 {
            break;
        }
    }

    Ok(ScriptResource {
        offset,
        size: index,
        value: ScriptResourceContent::Products(products),
    })
}

/// Get the table of commands.
fn get_commands_table(rom: &Rom) -> Result<&Vec<ScriptCommand>, ScriptResourceReadingError> {
    Ok(rom
        .refs
        .script_cmds
        .as_ref()
        .ok_or(ScriptResourceReadingError::ScriptsTableNotLoaded)?
        .commands
        .as_ref())
}

// ANCHOR Printing
pub(crate) struct ScriptResourceFormatter<'a> {
    /// The ROM to use for formatting.
    table: &'a Vec<ScriptCommand>,
    /// Whether to use colors.
    colors: bool,
    /// The formatting output (console or string)
    output: String,
    /// The alignment to make sure the commands' arguments are aligned
    cmd_align: Option<usize>,
}

impl<'a> ScriptResourceFormatter<'a> {
    /// Creates a new formatter for the given script resource.
    pub fn new(rom: &'a Rom, colors: bool) -> Self {
        let table = get_commands_table(rom).unwrap();
        let cmd_align = table.iter().map(|cmd| cmd.name.len()).max();

        Self {
            table,
            colors,
            output: String::new(),
            cmd_align,
        }
    }

    // ANCHOR Formatting types
    fn format_resource(&mut self, arg: &ScriptResource) {
        // @org ref_offset
        self.push_keyword("@org");
        self.push_space();
        self.push_ref(
            arg.offset,
            match arg.value {
                ScriptResourceContent::Script(_) => "Script",
                ScriptResourceContent::Movement(_) => "Movement",
                ScriptResourceContent::Text(_) => "Text",
                ScriptResourceContent::Products(_) => "Mart",
            },
        );
        self.push_comment(&format!("size: {}", arg.size));
        self.push_newline();

        // content
        match arg.value {
            ScriptResourceContent::Script(ref instructions) => {
                self.format_script(instructions);
            }
            _ => todo!(),
        }
    }

    fn format_script(&mut self, instructions: &[ScriptInstruction]) {
        for instruction in instructions {
            self.push_tab();
            // Get the instruction name
            let cmd = &self.table[instruction.code as usize];
            self.push_command(cmd);

            if instruction.args.len() != 0 {
                self.push_space();
            }

            for (i, arg) in instruction.args.iter().enumerate() {
                use ScriptArgument as A;
                match arg {
                    A::Byte(byte) => self.push_byte(*byte),
                    A::Half(half) => self.push_half(*half),
                    A::Word(word) => self.push_word(*word),
                    A::Flag(flag) => self.push_val(*flag, "Flag"),
                    A::Var(var) => self.push_val(*var, "Var"),

                    A::ScriptOffset(offset) => self.push_ref(*offset, "Script"),
                    A::MovementOffset(offset) => self.push_ref(*offset, "Movement"),
                    A::TextOffset(offset) => self.push_ref(*offset, "Text"),
                    A::ProductsOffset(offset) => self.push_ref(*offset, "Mart"),
                }

                if i != instruction.args.len() - 1 {
                    self.push_str(", ")
                }
            }

            self.push_newline()
        }
    }

    // ANCHOR Format primitives
    /// Push a keyword
    fn push_keyword(&mut self, keyword: &str) {
        match self.colors {
            false => self.push_str(keyword),
            true => self.push_str(keyword.bright_red().bold()),
        }
    }
    /// Push a command name
    fn push_command(&mut self, cmd: &ScriptCommand) {
        let name = &cmd.name;

        match self.colors {
            false => self.push_str(name),
            true => self.push_str(name.bright_yellow()),
        }

        if let Some(cmd_align) = self.cmd_align {
            let spaces = cmd_align - name.len();
            for _ in 0..spaces {
                self.push_space();
            }
        }
    }
    /// Push an offset (possibly with a prefix).
    fn push_ref(&mut self, offset: Offset, prefix: &str) {
        // Italic prefix_0FF5E7
        if prefix.len() != 0 {
            let offset = format!("{:07X}", offset);
            let res = format!("{}_{}", prefix, offset);
            match self.colors {
                true => self.push_str(res.italic().bright_blue().underline()),
                false => self.push_str(res),
            }
        }
        // Simple 0x0FF5E7
        else {
            self.push_word(offset as u32);
        }
    }
    /// Push a variable or a flag
    fn push_val(&mut self, value: u16, prefix: &str) {
        let value = format!("{}_{:04X}", prefix, value);
        match self.colors {
            true => self.push_str(value.bright_purple().italic()),
            false => self.push_str(value),
        }
    }

    /// Push a comment
    fn push_comment(&mut self, comment: &str) {
        match self.colors {
            true => {
                self.push_str(" # ".green().italic());
                self.push_str(comment.green().italic())
            }
            false => {
                self.push_str(" # ");
                self.push_str(comment)
            }
        }
    }
    /// Push a word
    fn push_byte(&mut self, byte: u8) {
        let word = format!("0x{:02X}", byte);
        match self.colors {
            true => self.push_str(word.bright_green()),
            false => self.push_str(word),
        }
    }
    /// Push a word
    fn push_half(&mut self, half: u16) {
        let word = format!("0x{:04X}", half);
        match self.colors {
            true => self.push_str(word.bright_green()),
            false => self.push_str(word),
        }
    }
    /// Push a word
    fn push_word(&mut self, word: u32) {
        let word = format!("0x{:08X}", word);
        match self.colors {
            true => self.push_str(word.bright_green()),
            false => self.push_str(word),
        }
    }
    /// Push something that is convertible to a string
    #[inline]
    fn push_str(&mut self, string: impl ToString) {
        self.output.push_str(&string.to_string())
    }
    /// Push a space character
    #[inline]
    fn push_space(&mut self) {
        self.output.push(' ')
    }
    /// Push a new line character
    #[inline]
    fn push_newline(&mut self) {
        self.output.push('\n')
    }
    /// Push a tabulation
    #[inline]
    fn push_tab(&mut self) {
        self.output.push_str("    ")
    }
}

impl ScriptResource {
    /// Converts the given script resource to a string.
    pub fn to_string(&self, rom: &Rom) -> String {
        let mut f = ScriptResourceFormatter::new(rom, false);
        f.format_resource(self);
        f.output
    }
    /// Converts the given script resource to a string, with colors.
    pub fn to_string_colors(&self, rom: &Rom) -> String {
        let mut f = ScriptResourceFormatter::new(rom, true);
        f.format_resource(self);
        f.output
    }
}
