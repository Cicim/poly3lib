//! The table of all scripts with their arguments.
use rom_data::{RomBase, RomIoError};

use crate::{maps::ProblemsLog, Rom, RomTable};

use super::{ScriptCommand, ScriptCommandsTable, ScriptReadInstruction};

/// Constructs a command given the name and a list of read instructions.
macro_rules! cmd {
    () => {
        ("unk", &[])
    };
    ($name:literal) => {
        ($name, &[])
    };
    ($name:literal $(, $args:ident)+) => {
        ($name, &[$(ScriptReadInstruction::$args, )*])
    };
}

type StaticScriptCommand = (&'static str, &'static [ScriptReadInstruction]);

/// Commands common to all RomBases
static CMDS_COMMON: &[StaticScriptCommand] = &[
    /*0x00*/ cmd!("nop"),
    /*0x01*/ cmd!("nop1"),
    /*0x02*/ cmd!("end", Stop),
    /*0x03*/ cmd!("return", Stop),
    /*0x04*/ cmd!("call", ScriptOffset, Stop),
    /*0x05*/ cmd!("goto", ScriptOffset, Stop),
    /*0x06*/ cmd!("goto_if", Byte, ScriptOffset),
    /*0x07*/ cmd!("call_if", Byte, ScriptOffset),
    /*0x08*/ cmd!("gotostd", Byte, Std, Stop),
    /*0x09*/ cmd!("callstd", Byte, Std),
    /*0x0A*/ cmd!("gotostd_if", Byte, Byte, Std),
    /*0x0B*/ cmd!("callstd_if", Byte, Byte, Std),
    /*0x0C*/ cmd!("gotoram", Stop),
    /*0x0D*/ cmd!("killscript", Stop),
    /*0x0E*/ cmd!("setmysteryeventstatus", Byte),
    /*0x0F*/ cmd!("loadword", Byte, TextOffsetIfStd),
    /*0x10*/ cmd!("loadbyte", Byte, Byte),
    /*0x11*/ cmd!("writebytetoaddr", Byte, Word),
    /*0x12*/ cmd!("loadbytefromaddr", Byte, Word),
    /*0x13*/ cmd!("setptrbyte", Byte, Word),
    /*0x14*/ cmd!("copylocal", Byte, Byte),
    /*0x15*/ cmd!("copybyte", Word, Word),
    /*0x16*/ cmd!("setvar", Var, Half),
    /*0x17*/ cmd!("addvar", Var, Half),
    /*0x18*/ cmd!("subvar", Var, Var),
    /*0x19*/ cmd!("copyvar", Var, Var),
    /*0x1A*/ cmd!("setorcopyvar", Var, Var),
    /*0x1B*/ cmd!("compare_local_to_local", Byte, Byte),
    /*0x1C*/ cmd!("compare_local_to_value", Byte, Byte),
    /*0x1D*/ cmd!("compare_local_to_addr", Byte, Word),
    /*0x1E*/ cmd!("compare_addr_to_local", Word, Byte),
    /*0x1F*/ cmd!("compare_addr_to_value", Word, Byte),
    /*0x20*/ cmd!("compare_addr_to_addr", Word, Word),
    /*0x21*/ cmd!("compare_var_to_value", Var, Half),
    /*0x22*/ cmd!("compare_var_to_var", Var, Var),
    /*0x23*/ cmd!("callnative", Word),
    /*0x24*/ cmd!("gotonative", Word, Stop),
    /*0x25*/ cmd!("special", Half),
    /*0x26*/ cmd!("specialvar", Var, Half),
    /*0x27*/ cmd!("waitstate"),
    /*0x28*/ cmd!("delay", Half),
    /*0x29*/ cmd!("setflag", Flag),
    /*0x2A*/ cmd!("clearflag", Flag),
    /*0x2B*/ cmd!("checkflag", Flag),
    /*0x2C*/ cmd!("initclock", Var, Var),
    /*0x2D*/ cmd!("dotimebasedevents"),
    /*0x2E*/ cmd!("gettime"),
    /*0x2F*/ cmd!("playse", Half),
    /*0x30*/ cmd!("waitse"),
    /*0x31*/ cmd!("playfanfare", Half),
    /*0x32*/ cmd!("waitfanfare"),
    /*0x33*/ cmd!("playbgm", Half, Byte),
    /*0x34*/ cmd!("savebgm", Half),
    /*0x35*/ cmd!("fadedefaultbgm"),
    /*0x36*/ cmd!("fadenewbgm", Half),
    /*0x37*/ cmd!("fadeoutbgm", Byte),
    /*0x38*/ cmd!("fadeinbgm", Byte),
    /*0x39*/ cmd!("warp", Byte, Byte, Byte, Var, Var),
    /*0x3A*/ cmd!("warpsilent", Byte, Byte, Byte, Var, Var),
    /*0x3B*/ cmd!("warpdoor", Byte, Byte, Byte, Var, Var),
    /*0x3C*/ cmd!("warphole", Byte, Byte),
    /*0x3D*/ cmd!("warpteleport", Byte, Byte, Byte, Var, Var),
    /*0x3E*/ cmd!("setwarp", Byte, Byte, Byte, Var, Var),
    /*0x3F*/ cmd!("setdynamicwarp", Byte, Byte, Byte, Var, Var),
    /*0x40*/ cmd!("setdivewarp", Byte, Byte, Byte, Var, Var),
    /*0x41*/ cmd!("setholewarp", Byte, Byte, Byte, Var, Var),
    /*0x42*/ cmd!("getplayerxy", Var, Var),
    /*0x43*/ cmd!("getpartysize"),
    /*0x44*/ cmd!("additem", Var, Var),
    /*0x45*/ cmd!("removeitem", Var, Var),
    /*0x46*/ cmd!("checkitemspace", Var, Var),
    /*0x47*/ cmd!("checkitem", Var, Var),
    /*0x48*/ cmd!("checkitemtype", Var),
    /*0x49*/ cmd!("addpcitem", Var, Var),
    /*0x4A*/ cmd!("checkpcitem", Var, Var),
    /*0x4B*/ cmd!("adddecoration", Var),
    /*0x4C*/ cmd!("removedecoration", Var),
    /*0x4D*/ cmd!("checkdecor", Var),
    /*0x4E*/ cmd!("checkdecorspace", Var),
    /*0x4F*/ cmd!("applymovement", Var, MovementOffset),
    /*0x50*/ cmd!("applymovement_at", Var, MovementOffset, Byte, Byte),
    /*0x51*/ cmd!("waitmovement", Var),
    /*0x52*/ cmd!("waitmovement_at", Var, Byte, Byte),
    /*0x53*/ cmd!("removeobject", Var),
    /*0x54*/ cmd!("removeobject_at", Var, Byte, Byte),
    /*0x55*/ cmd!("addobject", Var),
    /*0x56*/ cmd!("addobject_at", Var, Byte, Byte),
    /*0x57*/ cmd!("setobjectxy", Var, Var, Var),
    /*0x58*/ cmd!("showobjectat", Var, Byte, Byte),
    /*0x59*/ cmd!("hideobjectat", Var, Byte, Byte),
    /*0x5A*/ cmd!("faceplayer"),
    /*0x5B*/ cmd!("turnobject", Var, Byte),
    /*0x5C*/ cmd!("trainerbattle", TrainerBattle),
    /*0x5D*/ cmd!("trainerbattlebegin"),
    /*0x5E*/ cmd!("gotopostbattlescript"),
    /*0x5F*/ cmd!("gotobeatenscript"),
    /*0x60*/ cmd!("checktrainerflag", Var),
    /*0x61*/ cmd!("settrainerflag", Var),
    /*0x62*/ cmd!("cleartrainerflag", Var),
    /*0x63*/ cmd!("setobjectxyperm", Var, Var, Var),
    /*0x64*/ cmd!("moveobjectoffscreen", Var),
    /*0x65*/ cmd!("setobjectmovementtype", Var, Byte),
    /*0x66*/ cmd!("waitmessage"),
    /*0x67*/ cmd!("message", TextOffset),
    /*0x68*/ cmd!("closemessage"),
    /*0x69*/ cmd!("lockall"),
    /*0x6A*/ cmd!("lock"),
    /*0x6B*/ cmd!("releaseall"),
    /*0x6C*/ cmd!("release"),
    /*0x6D*/ cmd!("waitbuttonpress"),
    /*0x6E*/ cmd!("yesnobox", Byte, Byte),
    /*0x6F*/ cmd!("multichoice", Byte, Byte, Byte, Byte),
    /*0x70*/ cmd!("multichoicedefault", Byte, Byte, Byte, Byte, Byte),
    /*0x71*/ cmd!("multichoicegrid", Byte, Byte, Byte, Byte, Byte),
    /*0x72*/ cmd!("drawbox", Byte, Byte, Byte, Byte),
    /*0x73*/ cmd!("erasebox", Byte, Byte, Byte, Byte),
    /*0x74*/ cmd!("drawboxtext", Byte, Byte, Byte, Byte),
    /*0x75*/ cmd!("showmonpic", Var, Byte, Byte),
    /*0x76*/ cmd!("hidemonpic"),
    /*0x77*/ cmd!("showcontestwinner", Byte),
    /*0x78*/ cmd!("braillemessage", TextOffset),
    /*0x79*/ cmd!("givemon", Var, Byte, Var, Word, Word, Byte),
    /*0x7A*/ cmd!("giveegg", Var),
    /*0x7B*/ cmd!("setmonmove", Byte, Byte, Half),
    /*0x7C*/ cmd!("checkpartymove", Half),
    /*0x7D*/ cmd!("bufferspeciesname", Byte, Var),
    /*0x7E*/ cmd!("bufferleadmonspeciesname", Byte),
    /*0x7F*/ cmd!("bufferpartymonnick", Byte, Var),
    /*0x80*/ cmd!("bufferitemname", Byte, Var),
    /*0x81*/ cmd!("bufferdecorationname", Byte, Var),
    /*0x82*/ cmd!("buffermovename", Byte, Var),
    /*0x83*/ cmd!("buffernumberstring", Byte, Var),
    /*0x84*/ cmd!("bufferstdstring", Byte, Var),
    /*0x85*/ cmd!("bufferstring", Byte, Word),
    /*0x86*/ cmd!("pokemart", MartOffset),
    /*0x87*/ cmd!("pokemartdecoration", MartOffset),
    /*0x88*/ cmd!("pokemartdecoration2", MartOffset),
    /*0x89*/ cmd!("playslotmachine", Var),
    /*0x8A*/ cmd!("setberrytree", Byte, Byte, Byte),
    /*0x8B*/ cmd!("choosecontestmon"),
    /*0x8C*/ cmd!("startcontest"),
    /*0x8D*/ cmd!("showcontestresults"),
    /*0x8E*/ cmd!("contestlinktransfer"),
    /*0x8F*/ cmd!("random", Var),
    /*0x90*/ cmd!("addmoney", Word, Byte),
    /*0x91*/ cmd!("removemoney", Word, Byte),
    /*0x92*/ cmd!("checkmoney", Word, Byte),
    /*0x93*/ cmd!("showmoneybox", Byte, Byte, Byte),
    /*0x94*/ cmd!("hidemoneybox", Byte, Byte),
    /*0x95*/ cmd!("updatemoneybox", Byte, Byte, Byte),
    /*0x96*/ cmd!("getpricereduction", Var),
    /*0x97*/ cmd!("fadescreen", Byte),
    /*0x98*/ cmd!("fadescreenspeed", Byte, Byte),
    /*0x99*/ cmd!("setflashradius", Var),
    /*0x9A*/ cmd!("animateflash", Byte),
    /*0x9B*/ cmd!("messageautoscroll", TextOffset),
    /*0x9C*/ cmd!("dofieldeffect", Var),
    /*0x9D*/ cmd!("setfieldeffectargument", Byte, Var),
    /*0x9E*/ cmd!("waitfieldeffect", Var),
    /*0x9F*/ cmd!("setrespawn", Var),
    /*0xA0*/ cmd!("checkplayergender"),
    /*0xA1*/ cmd!("playmoncry", Var, Var),
    /*0xA2*/ cmd!("setmetatile", Var, Var, Var, Var),
    /*0xA3*/ cmd!("resetweather"),
    /*0xA4*/ cmd!("setweather", Var),
    /*0xA5*/ cmd!("doweather"),
    /*0xA6*/ cmd!("setstepcallback", Byte),
    /*0xA7*/ cmd!("setmaplayoutindex", Var),
    /*0xA8*/ cmd!("setobjectpriority", Var, Byte, Byte, Byte),
    /*0xA9*/ cmd!("resetobjectpriority", Var, Byte, Byte),
    /*0xAA*/ cmd!("createvobject", Byte, Byte, Var, Var, Byte, Byte),
    /*0xAB*/ cmd!("turnvobject", Byte, Byte),
    /*0xAC*/ cmd!("opendoor", Var, Var),
    /*0xAD*/ cmd!("closedoor", Var, Var),
    /*0xAE*/ cmd!("waitdooranim"),
    /*0xAF*/ cmd!("setdooropen", Var, Var),
    /*0xB0*/ cmd!("setdoorclosed", Var, Var),
    /*0xB1*/ cmd!("addelevmenuitem", Byte, Var, Var, Var),
    /*0xB2*/ cmd!("showelevmenu"),
    /*0xB3*/ cmd!("checkcoins", Var),
    /*0xB4*/ cmd!("addcoins", Var),
    /*0xB5*/ cmd!("removecoins", Var),
    /*0xB6*/ cmd!("setwildbattle", Half, Byte, Half),
    /*0xB7*/ cmd!("dowildbattle"),
    /*0xB8*/ cmd!("setvaddress", Word),
    /*0xB9*/ cmd!("vgoto", Word, Stop),
    /*0xBA*/ cmd!("vcall", Word),
    /*0xBB*/ cmd!("vgoto_if", Byte, Word),
    /*0xBC*/ cmd!("vcall_if", Byte, Word),
    /*0xBD*/ cmd!("vmessage", Word),
    /*0xBE*/ cmd!("vloadptr", Word),
    /*0xBF*/ cmd!("vbufferstring", Byte, Word),
    /*0xC0*/ cmd!("showcoinsbox", Byte, Byte),
    /*0xC1*/ cmd!("hidecoinsbox", Byte, Byte),
    /*0xC2*/ cmd!("updatecoinsbox", Byte, Byte),
    /*0xC3*/ cmd!("incrementgamestat", Byte),
    /*0xC4*/ cmd!("setescapewarp", Byte, Byte, Byte, Var, Var),
    /*0xC5*/ cmd!("waitmoncry"),
    /*0xC6*/ cmd!("bufferboxname", Byte, Var),
];
/// Specific commands for FireRed and LeafGreen
static CMDS_FRLG: &[StaticScriptCommand] = &[
    /*0xC7*/ cmd!("textcolor", Byte),
    /*0xC8*/ cmd!("loadhelp", TextOffset),
    /*0xC9*/ cmd!("unloadhelp"),
    /*0xCA*/ cmd!("signmsg"),
    /*0xCB*/ cmd!("normalmsg"),
    /*0xCC*/ cmd!("comparestat", Byte, Word),
    /*0xCD*/ cmd!("setmonmodernfatefulencounter", Var),
    /*0xCE*/ cmd!("checkmonmodernfatefulencounter", Var),
    /*0xCF*/ cmd!("trywondercardscript"),
    /*0xD0*/ cmd!("setworldmapflag", Half),
    /*0xD1*/ cmd!("warpspinenter", Byte, Byte, Byte, Var, Var),
    /*0xD2*/ cmd!("setmonmetlocation", Var, Byte),
    /*0xD3*/ cmd!("getbraillestringwidth", Word),
    /*0xD4*/ cmd!("bufferitemnameplural", Byte, Var, Var),
];
/// Specific Commands for Emerald.
static CMDS_EMERALD: &[StaticScriptCommand] = &[
    /*0xC7*/ cmd!(),
    /*0xC8*/ cmd!(),
    /*0xC9*/ cmd!(),
    /*0xCA*/ cmd!(),
    /*0xCB*/ cmd!(),
    /*0xCC*/ cmd!(),
    /*0xCD*/ cmd!("setmonmodernfatefulencounter", Var),
    /*0xCE*/ cmd!("checkmonmodernfatefulencounter", Var),
    /*0xCF*/ cmd!("trywondercardscript"),
    /*0xD0*/ cmd!(),
    /*0xD1*/ cmd!("warpspinenter", Byte, Byte, Byte, Var, Var),
    /*0xD2*/ cmd!("setmonmetlocation", Var, Byte),
    /*0xD3*/ cmd!("moverotatingtileobjects", Var),
    /*0xD4*/ cmd!("turnrotatingtileobjects"),
    /*0xD5*/ cmd!("initrotatingtilepuzzle", Var),
    /*0xD6*/ cmd!("freerotatingtilepuzzle"),
    /*0xD7*/ cmd!("warpmossdeepgym", Byte, Byte, Byte, Var, Var),
    /*0xD8*/ cmd!("selectapproachingtrainer"),
    /*0xD9*/ cmd!("lockfortrainer"),
    /*0xDA*/ cmd!("closebraillemessage"),
    /*0xDB*/ cmd!("messageinstant", TextOffset),
    /*0xDC*/ cmd!("fadescreenswapbuffers", Byte),
    /*0xDD*/ cmd!("buffertrainerclassname", Byte, Var),
    /*0xDE*/ cmd!("buffertrainername", Byte, Var),
    /*0xDF*/ cmd!("pokenavcall", TextOffset),
    /*0xE0*/ cmd!("warpwhitefade", Byte, Byte, Byte, Var, Var),
    /*0xE1*/ cmd!("buffercontestname", Byte, Var),
    /*0xE2*/ cmd!("bufferitemnameplural", Byte, Var, Var),
];

/// Read the script commands table from ROM.
pub(crate) fn read(rom: &Rom, log: &mut ProblemsLog) -> Result<ScriptCommandsTable, RomIoError> {
    // TODO Read from references first
    let (start_offset, end_offset) = match rom.base() {
        RomBase::FireRed => (0x15f9b4, 0x15fd08),
        RomBase::LeafGreen => (0x15f990, 0x15fce4),
        RomBase::Ruby => (0x14ae30, 0x14b148),
        RomBase::Sapphire => (0x14ae30, 0x14b148),
        RomBase::Emerald => (0x1db67c, 0x1dba08),
    };
    let start_references = rom.data.find_references(start_offset, 4);
    let end_references = rom.data.find_references(end_offset, 4);

    let mut commands = default_scripts_table(rom.base());
    let mut real_length = 0;

    // Get the commands
    if (end_offset as isize - start_offset as isize) < 0x80 {
        log.push_error(
            "script_cmds",
            "Invalid computed table length. Using default.",
        );
    } else {
        // Get the value at the second word
        let nop1_address = rom.data.read_word(start_offset + 4)?;
        // Read up to the table's length
        let length = (end_offset - start_offset) / 4;
        real_length = length;

        // Read only the commands you know how to read
        for i in 0..length.min(commands.len()) {
            match rom.data.read_word(start_offset + i * 4) {
                Ok(curr_address) => {
                    // If we have a nop not in position 1, push unknown.
                    if i > 1 && curr_address == nop1_address {
                        let unk_cmd = ScriptCommand {
                            name: format!("nop_{:02X}", i),
                            read: vec![],
                        };
                        commands[i] = unk_cmd;
                        continue;
                    }

                    // If we have an invalid offset for some reason
                    if !rom.data.is_pointer_valid(curr_address) {
                        let invalid_cmd = ScriptCommand {
                            name: format!("invalid_{:02X}", i),
                            read: vec![ScriptReadInstruction::Stop],
                        };
                        commands[i] = invalid_cmd;
                    }
                }
                Err(_) => log.push_warning(
                    "script_cmds",
                    "Table ends before provided length. Cutting off",
                ),
            }
        }

        // For all the others, add a unknown
        while commands.len() < length {
            commands.push(ScriptCommand {
                name: format!("unk_{:02X}", commands.len()),
                // REVIEW Stop when reading unknown instructions to avoid
                //        reading the wrong number of arguments.
                read: vec![ScriptReadInstruction::Stop],
            })
        }
    }

    Ok(ScriptCommandsTable {
        table: RomTable {
            offset: start_offset,
            length: real_length,
            references: start_references,
        },
        end_offset,
        end_references,
        commands,
    })
}

/// Gets the default scripts table for a rom type
fn default_scripts_table(base: RomBase) -> Vec<ScriptCommand> {
    let mut commands = Vec::new();

    // Get the other commands after the common ones
    let other_commands = match base {
        RomBase::Ruby | RomBase::Sapphire => &[],
        RomBase::FireRed | RomBase::LeafGreen => CMDS_FRLG,
        RomBase::Emerald => CMDS_EMERALD,
    };

    for cmd in CMDS_COMMON.iter().chain(other_commands) {
        let static_cmd = *cmd;
        let owned_cmd = static_to_command(static_cmd);
        commands.push(owned_cmd);
    }

    commands
}

/// Converts a table entry to a owned script command.
fn static_to_command(static_cmd: StaticScriptCommand) -> ScriptCommand {
    let (name, read) = static_cmd;

    ScriptCommand {
        name: name.to_owned(),
        read: read.to_owned(),
    }
}
