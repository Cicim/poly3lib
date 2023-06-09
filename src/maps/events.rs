use std::fmt::{self, Display, Formatter};

use gba_macro::gba_struct;
use gba_types::{vectors::VectorData, GBAIOError};
use serde::{Deserialize, Serialize};

use crate::{rom::Rom, scripts::ScriptResource};

// ANCHOR Events
/* In FireRed, there is a peculiarity in this struct: it needs an union to
 * represent the different types of events. In Emerald, this is not the case.
 *  union {
 *     struct {
 *         u8 elevation;
 *         u8 movementType;
 *         u16 movementRangeX:4;
 *         u16 movementRangeY:4;
 *         u16 trainerType;
 *         u16 trainerRange_berryTreeId;
 *     } normal;
 *     struct {
 *         u8 targetLocalId;
 *         u8 padding[3];
 *         u16 targetMapNum;
 *         u16 targetMapGroup;
 *     } clone;
 * } objUnion;
 */
gba_struct!(ObjectEventTemplate {
    u8 local_id;
    u8 graphics_id;
    u8 kind; // Always OBJ_KIND_NORMAL in Emerald.
    i16 x;
    i16 y;
    u8 elevation;
    u8 movement_type;
    u16 movement_range_x:4;
    u16 movement_range_y:4;
    u16 trainer_type;
    u16 trainer_range_berry_tree_id;
    void *script;
    u16 flag_id;
});

gba_struct!(WarpEvent {
    i16 x;
    i16 y;
    u8 elevation;
    u8 warp_id;
    u8 map_index;
    u8 map_group;
});

gba_struct!(CoordEvent {
    i16 x;
    i16 y;
    u8 elevation;
    u16 trigger;
    u16 index;
    void *script;
});

/* The data field has different meanings depending on the kind of event.
 * In Emerald and Ruby these are:
 * union {
 *     const u8 *script;
 *     struct {
 *         u16 item;
 *         u16 hiddenItemId;
 *     } hiddenItem;
 *     u32 secretBaseId;
 * } bgUnion;
 *
 * In FireRed they are
 * union {
 *     const u8 *script;
 *     struct {
 *        u32 item:16;
 *        u32 flag:8;
 *        u32 quantity:7;
 *        u32 underfoot:1;
 *     } hiddenItem;
 * } bgUnion;
 */
gba_struct!(BgEvent {
    u16 x;
    u16 y;
    u8 elevation;
    u8 kind;
    u32 data;
});

gba_struct!(MapEvents {
    u8 object_event_count;
    u8 warp_count;
    u8 coord_event_count;
    u8 bg_event_count;
    struct ObjectEventTemplate object_events{$object_event_count};
    struct WarpEvent warps{$warp_count};
    struct CoordEvent coord_events{$coord_event_count};
    struct BgEvent bg_events{$bg_event_count};
});

// ANCHOR Finding script offsets
pub(super) fn get_coord_event_scripts(vec: &VectorData<CoordEvent>) -> Vec<ScriptResource> {
    let mut res = vec![];

    // Extract the script offsets if possible.
    if let Some(events) = vec.get_data() {
        for event in events {
            if let Some(valid_offset) = event.script.offset() {
                res.push(ScriptResource::Script(valid_offset));
            }
        }
    }

    res
}

pub(super) fn get_object_event_scripts(
    vec: &VectorData<ObjectEventTemplate>,
) -> Vec<ScriptResource> {
    let mut res = vec![];

    // Extract the script offsets if possible.
    if let Some(events) = vec.get_data() {
        for event in events {
            if let Some(valid_offset) = event.script.offset() {
                res.push(ScriptResource::Script(valid_offset));
            }
        }
    }

    res
}

pub(super) fn get_bg_event_scripts(rom: &Rom, vec: &VectorData<BgEvent>) -> Vec<ScriptResource> {
    let mut res = vec![];

    // Extract the script offsets if possible.
    if let Some(events) = vec.get_data() {
        for event in events {
            // If the event is an event with a script associate to it
            // see BG_EVENT_PLAYER_FACING_{ANY, NORTH, SOUTH, EAST, WEST}
            if event.kind <= 4 {
                // Interpret event.data as a pointer to a script.
                if rom.is_pointer_valid(event.data) {
                    res.push(ScriptResource::Script(event.data as usize - 0x08000000));
                }
            }
        }
    }

    res
}

// ANCHOR Map Scripts
#[derive(Debug, Serialize, Deserialize)]
/// The variables to be compared to execute a script.
struct ConditionalScript {
    first_var: u16,
    second_var: u16,
    script: u32,
}

#[derive(Debug, Serialize, Deserialize)]
struct ConditionalScriptTable {
    offset: u32,
    read_size: usize,
    scripts: Vec<ConditionalScript>,
}

// Map Script types
const ON_LOAD: u8 = 1;
const ON_FRAME_TABLE: u8 = 2;
const ON_TRANSITION: u8 = 3;
const ON_WARP_INTO_MAP_TABLE: u8 = 4;
const ON_RESUME: u8 = 5;
const ON_DIVE_WARP: u8 = 6;
const ON_RETURN_TO_FIELD: u8 = 7;

#[derive(Debug, Serialize, Deserialize)]
pub struct MapScripts {
    read_size: usize,

    on_load: Option<u32>,
    on_frame_table: Option<ConditionalScriptTable>,
    on_transition: Option<u32>,
    on_warp_into_map_table: Option<ConditionalScriptTable>,
    on_resume: Option<u32>,
    on_dive_warp: Option<u32>,
    on_return_to_field: Option<u32>,
}

impl MapScripts {
    /// Reads the [`MapScripts`] from the ROM.
    pub fn read(rom: &Rom, offset: usize) -> Result<Self, GBAIOError> {
        let mut read_size = 0;

        // Script types
        let mut on_load = None;
        let mut on_frame_table = None;
        let mut on_transition = None;
        let mut on_warp_into_map_table = None;
        let mut on_resume = None;
        let mut on_dive_warp = None;
        let mut on_return_to_field = None;

        // While you have not arrived at the end of the table.
        loop {
            // Read the tag
            let tag = rom.read::<u8>(offset + read_size)?;
            read_size += 1;

            if tag == 0 || tag > 7 {
                break;
            }

            // Read the unaligned offset, whatever it is.
            let table_offset = rom.read_unaligned_offset(offset + read_size)?;
            read_size += 4;

            match tag {
                // Those that do not require reading the extra table
                ON_LOAD if on_load.is_none() => on_load = Some(table_offset),
                ON_TRANSITION if on_transition.is_none() => on_transition = Some(table_offset),
                ON_RESUME if on_resume.is_none() => on_resume = Some(table_offset),
                ON_DIVE_WARP if on_dive_warp.is_none() => on_dive_warp = Some(table_offset),
                ON_RETURN_TO_FIELD if on_return_to_field.is_none() => {
                    on_return_to_field = Some(table_offset)
                }

                // Those that require reading the extra table
                ON_FRAME_TABLE if on_frame_table.is_none() => {
                    let conditional = MapScripts::read_conditional_script(rom, table_offset)?;
                    on_frame_table = Some(conditional);
                }
                ON_WARP_INTO_MAP_TABLE if on_warp_into_map_table.is_none() => {
                    let conditional = MapScripts::read_conditional_script(rom, table_offset)?;
                    on_warp_into_map_table = Some(conditional);
                }

                // Sure this may be an error, but if we stop now we'll never be able to correct it.
                _ => {}
            };
        }

        Ok(MapScripts {
            read_size,

            on_load,
            on_frame_table,
            on_transition,
            on_warp_into_map_table,
            on_resume,
            on_dive_warp,
            on_return_to_field,
        })
    }

    /// Reads a conditional script table from the ROM.
    fn read_conditional_script(
        rom: &Rom,
        table_offset: u32,
    ) -> Result<ConditionalScriptTable, GBAIOError> {
        // The output vector of conditional scripts
        let mut res = vec![];

        let table_offset = table_offset as usize;
        let mut read_size = 0;
        loop {
            // Read the first variable
            let first_var = rom.read_unaligned_halfword(table_offset + read_size)?;
            read_size += 2;

            if first_var == 0 {
                break;
            }

            // Read the second variable
            let second_var = rom.read_unaligned_halfword(table_offset + read_size)?;
            read_size += 2;

            // Read the script
            let script = rom.read_unaligned_offset(table_offset + read_size)?;
            read_size += 4;

            res.push(ConditionalScript {
                first_var,
                second_var,
                script,
            });
        }

        Ok(ConditionalScriptTable {
            offset: table_offset as u32,
            read_size,
            scripts: res,
        })
    }

    /// Clears this map scripts table from the ROM.
    ///
    /// Returns a vector of the offsets of the scripts that still need clearing.
    pub fn clear(&self, rom: &mut Rom, offset: usize) -> Result<Vec<ScriptResource>, GBAIOError> {
        let mut scripts_to_clear = vec![];

        // Clear the direct scripts
        macro_rules! clear_script {
            ($field:ident) => {
                if let Some(offset) = self.$field {
                    scripts_to_clear.push(ScriptResource::Script(offset as usize));
                }
            };
        }
        clear_script!(on_load);
        clear_script!(on_transition);
        clear_script!(on_resume);
        clear_script!(on_dive_warp);
        clear_script!(on_return_to_field);

        // Clear the conditional scripts
        if let Some(table) = &self.on_frame_table {
            scripts_to_clear.push(ScriptResource::MapScriptsTable(table.offset as usize));
        }
        if let Some(table) = &self.on_warp_into_map_table {
            scripts_to_clear.push(ScriptResource::MapScriptsTable(table.offset as usize));
        }

        // Clear the table itself
        rom.clear(offset as usize, self.read_size as usize)?;

        // The scripts will be cleared later
        Ok(scripts_to_clear)
    }
}

impl Display for MapScripts {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Map Scripts ({:<2} bytes)", self.read_size)?;
        writeln!(f, "======================")?;

        if let Some(on_load) = self.on_load {
            writeln!(f, "On Load: ${:08X}", on_load)?;
        }

        if let Some(on_frame_table) = &self.on_frame_table {
            writeln!(
                f,
                "On Frame Table: ${:08X} ({} bytes)",
                on_frame_table.offset, on_frame_table.read_size
            )?;
            for script in &on_frame_table.scripts {
                writeln!(
                    f,
                    "    ${:04X} == ${:04X} => ${:08X}",
                    script.first_var, script.second_var, script.script
                )?;
            }
        }

        if let Some(on_transition) = self.on_transition {
            writeln!(f, "On Transition: ${:08X}", on_transition)?;
        }

        if let Some(on_warp_into_map_table) = &self.on_warp_into_map_table {
            writeln!(
                f,
                "On Warp Into Map Table: ${:08X} ({} bytes)",
                on_warp_into_map_table.offset, on_warp_into_map_table.read_size
            )?;
            for script in &on_warp_into_map_table.scripts {
                writeln!(
                    f,
                    "    ${:04X} == ${:04X} => ${:08X}",
                    script.first_var, script.second_var, script.script
                )?;
            }
        }

        if let Some(on_resume) = self.on_resume {
            writeln!(f, "On Resume: ${:08X}", on_resume)?;
        }

        if let Some(on_dive_warp) = self.on_dive_warp {
            writeln!(f, "On Dive Warp: ${:08X}", on_dive_warp)?;
        }

        if let Some(on_return_to_field) = self.on_return_to_field {
            writeln!(f, "On Return To Field: ${:08X}", on_return_to_field)?;
        }

        Ok(())
    }
}
