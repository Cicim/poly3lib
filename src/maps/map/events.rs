use rom_data::{
    rom_struct,
    types::{BitFields, RomArray, RomPointer, RomReadableType, RomSizedType, RomWritableType},
    Offset, RomBase, RomData, RomIoError,
};
use serde::{Deserialize, Serialize};

// ANCHOR Event structs
rom_struct!(MapEvents {
    u8 object_event_count;
    u8 warp_count;
    u8 coord_event_count;
    u8 bg_event_count;

    struct ObjectEvent object_events{$object_event_count};
    struct WarpEvent warps{$warp_count};
    struct CoordEvent coord_events{$coord_event_count};
    struct BgEvent bg_events{$bg_event_count};
});

impl MapEvents {
    /// Returns the warp events in this events table.
    pub fn get_warp_events(&self) -> &[WarpEvent] {
        self.warps.as_ref()
    }
    /// Returns the object events in this events table.
    pub fn get_object_events(&self) -> &[ObjectEvent] {
        self.object_events.as_ref()
    }
    /// Returns the coordinate events in this events table.
    pub fn get_coord_events(&self) -> &[CoordEvent] {
        self.coord_events.as_ref()
    }
    /// Returns the background events in this events table.
    pub fn get_bg_events(&self) -> &[BgEvent] {
        self.bg_events.as_ref()
    }
}

rom_struct!(WarpEvent {
    i16 x, y;
    u8 z;
    u8 warp_id;
    u8 map_index;
    u8 map_group;
});
rom_struct!(CoordEvent {
    i16 x, y;
    u8 z;
    u16 trigger;
    u16 index;
    void *script;
});

// ANCHOR Object events
rom_struct!(ObjectEventUnqualified {
    u8 local_id;
    u8 graphics_id;
    u8 kind;
    i16 x, y;
    // Kind-specific fields
    u8 data[8];

    void *script;
    u16 flag_id;
} priv);

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ObjectEvent {
    local_id: u8,
    graphics_id: u8,
    kind: u8,
    x: i16,
    y: i16,
    data: ObjectEventData,
    script: RomPointer,
    flag_id: u16,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ObjectEventData {
    // u8 elevation;
    // u8 movementType;
    // u16 movementRangeX:4;
    // u16 movementRangeY:4;
    // u16 trainerType;
    // u16 trainerRange_berryTreeId;
    /// When `kind == 0`
    Normal {
        z: u8,
        movement_type: u8,
        // u16 movement_range_x:4;
        movement_range_x: u8,
        // u16 movement_range_y:4;
        movement_range_y: u8,
        trainer_type: u16,
        trainer_range_berry_tree_id: u16,
    },
    /// When `kind == 1`
    Clone {
        target_local_id: u32,
        target_map_num: u16,
        target_map_group: u16,
    },

    /// When kind is anything else
    Unknown([u8; 8]),
}

impl RomSizedType for ObjectEvent {
    fn get_size(rom: &RomData) -> usize {
        ObjectEventUnqualified::get_size(rom)
    }
    fn get_alignment(rom: &RomData) -> usize {
        ObjectEventUnqualified::get_alignment(rom)
    }
}
impl RomReadableType for ObjectEvent {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        // Read the unqualified struct
        let read = ObjectEventUnqualified::read_from(rom, offset)?;
        // Get the data from which you're going to parse the fields
        let data: [u8; 8] = read.data.as_ref().try_into().unwrap();

        // Match on its kind
        let data = match read.kind {
            0 => {
                let z = data[0];
                let movement_type = data[1];

                let movement_range_x = (data[2] & 0b11110000) >> 4;
                let movement_range_y = data[2] & 0b1111;

                let trainer_type = u16::from_le_bytes([data[4], data[5]]);
                let trainer_range_berry_tree_id = u16::from_le_bytes([data[6], data[7]]);

                ObjectEventData::Normal {
                    z,
                    movement_type,
                    movement_range_x,
                    movement_range_y,
                    trainer_type,
                    trainer_range_berry_tree_id,
                }
            }
            255 => {
                // Clone
                let target_local_id = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
                let target_map_num = u16::from_le_bytes([data[4], data[5]]);
                let target_map_group = u16::from_le_bytes([data[6], data[7]]);

                ObjectEventData::Clone {
                    target_local_id,
                    target_map_num,
                    target_map_group,
                }
            }

            _ => {
                // Unknown kind
                ObjectEventData::Unknown(data)
            }
        };

        Ok(Self {
            local_id: read.local_id,
            graphics_id: read.graphics_id,
            kind: read.kind,
            x: read.x,
            y: read.y,
            data,
            script: read.script,
            flag_id: read.flag_id,
        })
    }
}
impl RomWritableType for ObjectEvent {
    fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        let data = match self.data {
            ObjectEventData::Normal {
                z,
                movement_type,
                movement_range_x,
                movement_range_y,
                trainer_type,
                trainer_range_berry_tree_id,
            } => {
                let mut data = [0; 8];
                data[0] = z;
                data[1] = movement_type;
                data[2] = movement_range_x << 4 | movement_range_y;
                data[4..6].copy_from_slice(&trainer_type.to_le_bytes());
                data[6..8].copy_from_slice(&trainer_range_berry_tree_id.to_le_bytes());
                data
            }
            ObjectEventData::Clone {
                target_local_id,
                target_map_num,
                target_map_group,
            } => {
                let mut data = [0; 8];
                data[0..4].copy_from_slice(&target_local_id.to_le_bytes());
                data[4..6].copy_from_slice(&target_map_num.to_le_bytes());
                data[6..8].copy_from_slice(&target_map_group.to_le_bytes());
                data
            }
            ObjectEventData::Unknown(dat) => dat,
        };

        let write = ObjectEventUnqualified {
            local_id: self.local_id,
            graphics_id: self.graphics_id,
            kind: self.kind,
            x: self.x,
            y: self.y,
            data: RomArray::from(data),
            script: self.script,
            flag_id: self.flag_id,
        };

        write.write_to(rom, offset)
    }
}

// ANCHOR Background events
rom_struct!(BgEventUnqualified {
    u16 x;
    u16 y;
    u8 z;
    u8 kind;

    // Kind-specific fields
    u32 data;
} priv);

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum BgEventData {
    Script(RomPointer),
    HiddenItemRSE {
        item: u16,
        hidden_item_id: u16,
    },
    HiddenItemFrLg {
        //  u32 item:16;
        item: u16,
        //  u32 flag:8;
        flag: u8,
        //  u32 quantity:7;
        quantity: u8,
        //  u32 underfoot:1;
        underfoot: bool,
    },
    SecretBaseId(u32),

    Unknown(u32),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct BgEvent {
    x: u16,
    y: u16,
    z: u8,
    kind: u8,
    data: BgEventData,
}

impl RomSizedType for BgEvent {
    fn get_size(rom: &RomData) -> usize {
        BgEventUnqualified::get_size(rom)
    }
    fn get_alignment(rom: &RomData) -> usize {
        BgEventUnqualified::get_alignment(rom)
    }
}
impl RomReadableType for BgEvent {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        // Read the unqualified struct
        let read: BgEventUnqualified = rom.read(offset)?;
        let kind = read.kind;

        let data = match (kind, rom.base) {
            // Script
            (0..=4, _) => {
                let ptr = RomPointer::from_pointer(read.data, rom);
                BgEventData::Script(ptr)
            }

            // Hidden Item
            (7, RomBase::Emerald | RomBase::Ruby | RomBase::Sapphire) => {
                let item = read.data as u16;
                let hidden_item_id = (read.data >> 16) as u16;
                BgEventData::HiddenItemRSE {
                    item,
                    hidden_item_id,
                }
            }
            (7, RomBase::FireRed | RomBase::LeafGreen) => {
                let fields: BitFields<u32, 4> = BitFields::new([16, 8, 7, 1]);
                let [item, flag, quantity, underfoot] = fields.read(read.data);
                BgEventData::HiddenItemFrLg {
                    item: item as u16,
                    flag: flag as u8,
                    quantity: quantity as u8,
                    underfoot: underfoot != 0,
                }
            }

            // Secret Base
            (8, RomBase::Emerald | RomBase::Ruby | RomBase::Sapphire) => {
                BgEventData::SecretBaseId(read.data)
            }

            // Unknown
            (_, _) => BgEventData::Unknown(read.data),
        };

        Ok(Self {
            x: read.x,
            y: read.y,
            z: read.z,
            kind,
            data,
        })
    }
}
impl RomWritableType for BgEvent {
    fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        let data = match self.data {
            BgEventData::Script(x) => x.pointer(),
            BgEventData::HiddenItemRSE {
                item,
                hidden_item_id,
            } => (hidden_item_id as u32) << 16 | (item as u32),
            BgEventData::HiddenItemFrLg {
                item,
                flag,
                quantity,
                underfoot,
            } => {
                let fields: BitFields<u32, 4> = BitFields::new([16, 8, 7, 1]);
                let data = [item as u32, flag as u32, quantity as u32, underfoot as u32];
                fields.write(0, data)
            }
            BgEventData::SecretBaseId(x) => x,
            BgEventData::Unknown(x) => x,
        };

        let write = BgEventUnqualified {
            x: self.x,
            y: self.y,
            z: self.z,
            kind: self.kind,
            data,
        };
        write.write_to(rom, offset)
    }
}

trait ScriptOffset {
    /// Read the offsets of the script referenced by this event (if any).
    fn script_offset(&self) -> Option<Offset>;
}

impl ScriptOffset for CoordEvent {
    fn script_offset(&self) -> Option<Offset> {
        self.script.offset()
    }
}
impl ScriptOffset for ObjectEvent {
    fn script_offset(&self) -> Option<Offset> {
        self.script.offset()
    }
}
impl ScriptOffset for BgEvent {
    fn script_offset(&self) -> Option<Offset> {
        match self.data {
            BgEventData::Script(ref x) => x.offset(),
            _ => None,
        }
    }
}

impl MapEvents {
    /// Get the scripts' offsets referenced by these events.
    pub fn get_scripts(&self) -> Vec<Offset> {
        let mut output = Vec::new();

        push_events(self.get_object_events(), &mut output);
        push_events(self.get_coord_events(), &mut output);
        push_events(self.get_bg_events(), &mut output);

        output
    }
}

/// Pushes events that can have scripts to the vector
fn push_events(events: &[impl ScriptOffset], output: &mut Vec<Offset>) {
    for event in events {
        if let Some(offset) = event.script_offset() {
            output.push(offset);
        }
    }
}

// ANCHOR Clearing events
impl MapEvents {
    /// Clears the events from the ROM.
    pub fn clear(mut self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        // Clear all the inner vectors
        self.object_events.to_clear();
        self.coord_events.to_clear();
        self.bg_events.to_clear();
        self.warps.to_clear();

        // Write the cleared events table
        self.write_to(rom, offset)?;
        // Clear the events table
        rom.clear_bytes(offset, Self::get_size(rom))
    }
}
