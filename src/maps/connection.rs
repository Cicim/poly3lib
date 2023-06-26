use gba_macro::gba_struct;

gba_struct!(MapConnection {
    u8 direction;
    u32 offset;
    u8 map_group;
    u8 map_index;
});

gba_struct!(MapConnections {
    i32 count;
    struct MapConnection connections{$count};
});
