use gba_macro::gba_struct;

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
