use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};

use rom_data::{Offset, RomIoError};

use crate::{Rom, RomTable};

use super::ProblemsLog;

mod resources;
pub use resources::*;

mod table;

mod graph;
pub use graph::ScriptGraph;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ScriptCommand {
    /// The command name.
    pub name: String,
    /// The instructions for the parser to read this instruction.
    pub read: Vec<ScriptReadInstruction>,
}

#[derive(Debug, Serialize_repr, Deserialize_repr, Clone)]
#[repr(u8)]
pub enum ScriptReadInstruction {
    /// The next argument is a byte.
    Byte = 1,
    /// The next argument is a halfword (2 bytes).
    Half = 2,
    /// The next argument is a word (4 bytes).
    Word = 3,
    /// The next argument is a flag (2 bytes).
    Flag = 8,
    /// The next argument is a var (2 bytes).
    Var = 9,
    /// This command calls `BattleSetup_ConfigureTrainerBattle` to
    /// read its arguments, so we have to read the next byte to
    /// determine the arguments configuration.
    TrainerBattle = 10,

    /// The next argument is the offset to another script (4 bytes).
    ///
    /// The offset should be valid and in ROM and the referenced
    /// script should be read recursively.
    ScriptOffset = 64,
    /// The next argument is the offset to movement data (4 bytes).
    ///
    /// The offset must be in valid and in ROM and the referenced
    /// data will be read as well.
    MovementOffset = 65,
    /// The next argument is the offset to text data (4 bytes).
    ///
    /// The offset must be valid and in ROM and the referenced
    /// data will be read as well.
    TextOffset = 66,
    /// The next argument is the offset to shop data (4 bytes).
    ///
    /// The offset must be valid and in ROM and the referenced
    /// data will be read as well.
    ProductsOffset = 67,

    /// The next argument is a text offset only if the next command
    /// calls an std function, otherwise it's a word.
    TextOffsetIfStd = 129,

    /// The script calls an std function.
    Std = 254,
    /// The script stops exection here for certain.
    Stop = 255,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ScriptCommandsTable {
    pub table: RomTable,
    /// The offset to the end of the table
    pub end_offset: Offset,
    /// The references to the end of the table
    pub end_references: Vec<Offset>,

    /// The commands in the table.
    pub commands: Vec<ScriptCommand>,
}

impl ScriptCommandsTable {
    /// Returns the length of the script table
    pub fn len(&self) -> u8 {
        self.commands.len() as u8
    }
}

// ANCHOR Methods
impl Rom {
    /// Clears the given scripts (and all referenced scripts and resources).
    ///
    /// Assumes the references to these scripts have already been cleared, so
    /// that some of the scripts may have no references in ROM.
    pub fn clear_scripts(&mut self, scripts: &[Offset]) -> Result<(), RomIoError> {
        // Convert the offsets to script markers
        let scripts = scripts
            .iter()
            .map(|offset| ScriptResourceMarker::Script(*offset))
            .collect::<Vec<_>>();

        // Construct the graph
        let graph = ScriptGraph::read(self, &scripts);

        // Clear the scripts
        graph.clear(self)
    }
}

/// Initialize the script commands table.
pub fn init_table(rom: &mut Rom, log: &mut ProblemsLog) -> Result<(), RomIoError> {
    if rom.refs.script_cmds.is_some() {
        return Ok(());
    }

    let table = self::table::read(rom, log)?;
    rom.refs.script_cmds = Some(table);

    Ok(())
}
