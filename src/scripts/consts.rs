/// Bytes to skip for each command.
pub static BYTES_TO_SKIP: [u32; 0x100] = [
    /* 0x00 --                            nop */ 0,
    /* 0x01 --                           nop1 */ 0,
    /* 0x02 --                            end */ 0,
    /* 0x03 --                         return */ 0,
    /* 0x04 --                           call */ 4,
    /* 0x05 --                           goto */ 4,
    /* 0x06 --                        goto_if */ 5,
    /* 0x07 --                        call_if */ 5,
    /* 0x08 --                        gotostd */ 1,
    /* 0x09 --                        callstd */ 1,
    /* 0x0A --                     gotostd_if */ 2,
    /* 0x0B --                     callstd_if */ 2,
    /* 0x0C --                      returnram */ 0,
    /* 0x0D --                         endram */ 0,
    /* 0x0E --          setmysteryeventstatus */ 1,
    /* 0x0F --                       loadword */ 5,
    /* 0x10 --                       loadbyte */ 2,
    /* 0x11 --                         setptr */ 5,
    /* 0x12 --                loadbytefromptr */ 5,
    /* 0x13 --                     setptrbyte */ 5,
    /* 0x14 --                      copylocal */ 2,
    /* 0x15 --                       copybyte */ 8,
    /* 0x16 --                         setvar */ 4,
    /* 0x17 --                         addvar */ 4,
    /* 0x18 --                         subvar */ 4,
    /* 0x19 --                        copyvar */ 4,
    /* 0x1A --                   setorcopyvar */ 4,
    /* 0x1B --         compare_local_to_local */ 2,
    /* 0x1C --         compare_local_to_value */ 2,
    /* 0x1D --           compare_local_to_ptr */ 5,
    /* 0x1E --           compare_ptr_to_local */ 5,
    /* 0x1F --           compare_ptr_to_value */ 5,
    /* 0x20 --             compare_ptr_to_ptr */ 8,
    /* 0x21 --           compare_var_to_value */ 4,
    /* 0x22 --             compare_var_to_var */ 4,
    /* 0x23 --                     callnative */ 4,
    /* 0x24 --                     gotonative */ 4,
    /* 0x25 --                        special */ 2,
    /* 0x26 --                     specialvar */ 4,
    /* 0x27 --                      waitstate */ 0,
    /* 0x28 --                          delay */ 2,
    /* 0x29 --                        setflag */ 2,
    /* 0x2A --                      clearflag */ 2,
    /* 0x2B --                      checkflag */ 2,
    /* 0x2C --                      initclock */ 4,
    /* 0x2D --              dotimebasedevents */ 0,
    /* 0x2E --                        gettime */ 0,
    /* 0x2F --                         playse */ 2,
    /* 0x30 --                         waitse */ 0,
    /* 0x31 --                    playfanfare */ 2,
    /* 0x32 --                    waitfanfare */ 0,
    /* 0x33 --                        playbgm */ 3,
    /* 0x34 --                        savebgm */ 2,
    /* 0x35 --                 fadedefaultbgm */ 0,
    /* 0x36 --                     fadenewbgm */ 2,
    /* 0x37 --                     fadeoutbgm */ 1,
    /* 0x38 --                      fadeinbgm */ 1,
    /* 0x39 --                           warp */ 7,
    /* 0x3A --                     warpsilent */ 7,
    /* 0x3B --                       warpdoor */ 7,
    /* 0x3C --                       warphole */ 2,
    /* 0x3D --                   warpteleport */ 7,
    /* 0x3E --                        setwarp */ 7,
    /* 0x3F --                 setdynamicwarp */ 7,
    /* 0x40 --                    setdivewarp */ 7,
    /* 0x41 --                    setholewarp */ 7,
    /* 0x42 --                    getplayerxy */ 4,
    /* 0x43 --                   getpartysize */ 0,
    /* 0x44 --                        additem */ 4,
    /* 0x45 --                     removeitem */ 4,
    /* 0x46 --                 checkitemspace */ 4,
    /* 0x47 --                      checkitem */ 4,
    /* 0x48 --                  checkitemtype */ 2,
    /* 0x49 --                      addpcitem */ 4,
    /* 0x4A --                    checkpcitem */ 4,
    /* 0x4B --                  adddecoration */ 2,
    /* 0x4C --               removedecoration */ 2,
    /* 0x4D --                     checkdecor */ 2,
    /* 0x4E --                checkdecorspace */ 2,
    /* 0x4F --                  applymovement */ 6,
    /* 0x50 --                  applymovement */ 8,
    /* 0x51 --                   waitmovement */ 2,
    /* 0x52 --                   waitmovement */ 4,
    /* 0x53 --                   removeobject */ 2,
    /* 0x54 --                   removeobject */ 4,
    /* 0x55 --                      addobject */ 2,
    /* 0x56 --                      addobject */ 4,
    /* 0x57 --                    setobjectxy */ 6,
    /* 0x58 --                   showobjectat */ 4,
    /* 0x59 --                   hideobjectat */ 4,
    /* 0x5A --                     faceplayer */ 0,
    /* 0x5B --                     turnobject */ 3,
    /* 0x5C ------------------- trainerbattle */ 0xFF, // Variable
    /* 0x5D --                dotrainerbattle */ 0,
    /* 0x5E --           gotopostbattlescript */ 0,
    /* 0x5F --               gotobeatenscript */ 0,
    /* 0x60 --               checktrainerflag */ 2,
    /* 0x61 --                 settrainerflag */ 2,
    /* 0x62 --               cleartrainerflag */ 2,
    /* 0x63 --                setobjectxyperm */ 6,
    /* 0x64 --             copyobjectxytoperm */ 2,
    /* 0x65 --          setobjectmovementtype */ 3,
    /* 0x66 --                    waitmessage */ 0,
    /* 0x67 --                        message */ 4,
    /* 0x68 --                   closemessage */ 0,
    /* 0x69 --                        lockall */ 0,
    /* 0x6A --                           lock */ 0,
    /* 0x6B --                     releaseall */ 0,
    /* 0x6C --                        release */ 0,
    /* 0x6D --                waitbuttonpress */ 0,
    /* 0x6E --                       yesnobox */ 2,
    /* 0x6F --                    multichoice */ 4,
    /* 0x70 --             multichoicedefault */ 5,
    /* 0x71 --                multichoicegrid */ 5,
    /* 0x72 --                        drawbox */ 0,
    /* 0x73 --                       erasebox */ 4,
    /* 0x74 --                    drawboxtext */ 4,
    /* 0x75 --                     showmonpic */ 4,
    /* 0x76 --                     hidemonpic */ 0,
    /* 0x77 --            showcontestpainting */ 1,
    /* 0x78 --                 braillemessage */ 4,
    /* 0x79 --                        givemon */ 14,
    /* 0x7A --                        giveegg */ 2,
    /* 0x7B --                     setmonmove */ 4,
    /* 0x7C --                 checkpartymove */ 2,
    /* 0x7D --              bufferspeciesname */ 3,
    /* 0x7E --       bufferleadmonspeciesname */ 1,
    /* 0x7F --             bufferpartymonnick */ 3,
    /* 0x80 --                 bufferitemname */ 3,
    /* 0x81 --           bufferdecorationname */ 3,
    /* 0x82 --                 buffermovename */ 3,
    /* 0x83 --             buffernumberstring */ 3,
    /* 0x84 --                bufferstdstring */ 3,
    /* 0x85 --                   bufferstring */ 5,
    /* 0x86 --                       pokemart */ 4,
    /* 0x87 --             pokemartdecoration */ 4,
    /* 0x88 --            pokemartdecoration2 */ 4,
    /* 0x89 --                playslotmachine */ 2,
    /* 0x8A --                   setberrytree */ 3,
    /* 0x8B --               choosecontestmon */ 0,
    /* 0x8C --                   startcontest */ 0,
    /* 0x8D --             showcontestresults */ 0,
    /* 0x8E --            contestlinktransfer */ 0,
    /* 0x8F --                         random */ 2,
    /* 0x90 --                       addmoney */ 5,
    /* 0x91 --                    removemoney */ 5,
    /* 0x92 --                     checkmoney */ 5,
    /* 0x93 --                   showmoneybox */ 3,
    /* 0x94 --                   hidemoneybox */ 2,
    /* 0x95 --                 updatemoneybox */ 3,
    /* 0x96 --              getpokenewsactive */ 2,
    /* 0x97 --                     fadescreen */ 1,
    /* 0x98 --                fadescreenspeed */ 2,
    /* 0x99 --                  setflashlevel */ 2,
    /* 0x9A --                   animateflash */ 1,
    /* 0x9B --              messageautoscroll */ 4,
    /* 0x9C --                  dofieldeffect */ 2,
    /* 0x9D --         setfieldeffectargument */ 3,
    /* 0x9E --                waitfieldeffect */ 2,
    /* 0x9F --                     setrespawn */ 2,
    /* 0xA0 --              checkplayergender */ 0,
    /* 0xA1 --                     playmoncry */ 4,
    /* 0xA2 --                    setmetatile */ 8,
    /* 0xA3 --                   resetweather */ 0,
    /* 0xA4 --                     setweather */ 2,
    /* 0xA5 --                      doweather */ 0,
    /* 0xA6 --                setstepcallback */ 1,
    /* 0xA7 --              setmaplayoutindex */ 2,
    /* 0xA8 --           setobjectsubpriority */ 5,
    /* 0xA9 --         resetobjectsubpriority */ 4,
    /* 0xAA --                  createvobject */ 8,
    /* 0xAB --                    turnvobject */ 2,
    /* 0xAC --                       opendoor */ 4,
    /* 0xAD --                      closedoor */ 4,
    /* 0xAE --                   waitdooranim */ 0,
    /* 0xAF --                    setdooropen */ 4,
    /* 0xB0 --                  setdoorclosed */ 4,
    /* 0xB1 --                addelevmenuitem */ 7,
    /* 0xB2 --                   showelevmenu */ 0,
    /* 0xB3 --                     checkcoins */ 2,
    /* 0xB4 --                       addcoins */ 2,
    /* 0xB5 --                    removecoins */ 2,
    /* 0xB6 --                  setwildbattle */ 5,
    /* 0xB7 --                   dowildbattle */ 0,
    /* 0xB8 --                    setvaddress */ 4,
    /* 0xB9 --                          vgoto */ 4,
    /* 0xBA --                          vcall */ 4,
    /* 0xBB --                       vgoto_if */ 5,
    /* 0xBC --                       vcall_if */ 5,
    /* 0xBD --                       vmessage */ 4,
    /* 0xBE --                 vbuffermessage */ 4,
    /* 0xBF --                  vbufferstring */ 5,
    /* 0xC0 --                   showcoinsbox */ 2,
    /* 0xC1 --                   hidecoinsbox */ 2,
    /* 0xC2 --                 updatecoinsbox */ 2,
    /* 0xC3 --              incrementgamestat */ 1,
    /* 0xC4 --                  setescapewarp */ 7,
    /* 0xC5 --                     waitmoncry */ 0,
    /* 0xC6 --                  bufferboxname */ 3,
    /* 0xC7 --                      textcolor */ 1,
    /* 0xC8 --                       loadhelp */ 4,
    /* 0xC9 --                     unloadhelp */ 0,
    /* 0xCA --                        signmsg */ 0,
    /* 0xCB --                      normalmsg */ 0,
    /* 0xCC --               comparehiddenvar */ 5,
    /* 0xCD --      setmodernfatefulencounter */ 2,
    /* 0xCE --    checkmodernfatefulencounter */ 2,
    /* 0xCF --            trywondercardscript */ 0,
    /* 0xD0 --                setworldmapflag */ 2,
    /* 0xD1 --                  warpspinenter */ 7,
    /* 0xD2 --              setmonmetlocation */ 3,
    /* 0xD3 --        moverotatingtileobjects */ 2,
    /* 0xD4 --        turnrotatingtileobjects */ 0,
    /* 0xD5 --         initrotatingtilepuzzle */ 2,
    /* 0xD6 --         freerotatingtilepuzzle */ 0,
    /* 0xD7 --                warpmossdeepgym */ 7,
    /* 0xD8 --       selectapproachingtrainer */ 0,
    /* 0xD9 --                 lockfortrainer */ 0,
    /* 0xDA --            closebraillemessage */ 0,
    /* 0xDB --                 messageinstant */ 4,
    /* 0xDC --          fadescreenswapbuffers */ 1,
    /* 0xDD --         buffertrainerclassname */ 3,
    /* 0xDE --              buffertrainername */ 3,
    /* 0xDF --                    pokenavcall */ 4,
    /* 0xE0 --                  warpwhitefade */ 7,
    /* 0xE1 --              buffercontestname */ 3,
    /* 0xE2 --           bufferitemnameplural */ 5, // end
    // Missing scripts
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];

/// Names of each command
pub static SCRIPT_NAMES: [&str; 0x100] = [
    /*0x00*/ "nop",
    /*0x01*/ "nop1",
    /*0x02*/ "end",
    /*0x03*/ "return",
    /*0x04*/ "call",
    /*0x05*/ "goto",
    /*0x06*/ "goto_if",
    /*0x07*/ "call_if",
    /*0x08*/ "gotostd",
    /*0x09*/ "callstd",
    /*0x0A*/ "gotostd_if",
    /*0x0B*/ "callstd_if",
    /*0x0C*/ "returnram",
    /*0x0D*/ "endram",
    /*0x0E*/ "setmysteryeventstatus",
    /*0x0F*/ "loadword",
    /*0x10*/ "loadbyte",
    /*0x11*/ "setptr",
    /*0x12*/ "loadbytefromptr",
    /*0x13*/ "setptrbyte",
    /*0x14*/ "copylocal",
    /*0x15*/ "copybyte",
    /*0x16*/ "setvar",
    /*0x17*/ "addvar",
    /*0x18*/ "subvar",
    /*0x19*/ "copyvar",
    /*0x1A*/ "setorcopyvar",
    /*0x1B*/ "compare_local_to_local",
    /*0x1C*/ "compare_local_to_value",
    /*0x1D*/ "compare_local_to_ptr",
    /*0x1E*/ "compare_ptr_to_local",
    /*0x1F*/ "compare_ptr_to_value",
    /*0x20*/ "compare_ptr_to_ptr",
    /*0x21*/ "compare_var_to_value",
    /*0x22*/ "compare_var_to_var",
    /*0x23*/ "callnative",
    /*0x24*/ "gotonative",
    /*0x25*/ "special",
    /*0x26*/ "specialvar",
    /*0x27*/ "waitstate",
    /*0x28*/ "delay",
    /*0x29*/ "setflag",
    /*0x2A*/ "clearflag",
    /*0x2B*/ "checkflag",
    /*0x2C*/ "initclock",
    /*0x2D*/ "dotimebasedevents",
    /*0x2E*/ "gettime",
    /*0x2F*/ "playse",
    /*0x30*/ "waitse",
    /*0x31*/ "playfanfare",
    /*0x32*/ "waitfanfare",
    /*0x33*/ "playbgm",
    /*0x34*/ "savebgm",
    /*0x35*/ "fadedefaultbgm",
    /*0x36*/ "fadenewbgm",
    /*0x37*/ "fadeoutbgm",
    /*0x38*/ "fadeinbgm",
    /*0x39*/ "warp",
    /*0x3A*/ "warpsilent",
    /*0x3B*/ "warpdoor",
    /*0x3C*/ "warphole",
    /*0x3D*/ "warpteleport",
    /*0x3E*/ "setwarp",
    /*0x3F*/ "setdynamicwarp",
    /*0x40*/ "setdivewarp",
    /*0x41*/ "setholewarp",
    /*0x42*/ "getplayerxy",
    /*0x43*/ "getpartysize",
    /*0x44*/ "additem",
    /*0x45*/ "removeitem",
    /*0x46*/ "checkitemspace",
    /*0x47*/ "checkitem",
    /*0x48*/ "checkitemtype",
    /*0x49*/ "addpcitem",
    /*0x4A*/ "checkpcitem",
    /*0x4B*/ "adddecoration",
    /*0x4C*/ "removedecoration",
    /*0x4D*/ "checkdecor",
    /*0x4E*/ "checkdecorspace",
    /*0x4F*/ "applymovement",
    /*0x50*/ "applymovement",
    /*0x51*/ "waitmovement",
    /*0x52*/ "waitmovement",
    /*0x53*/ "removeobject",
    /*0x54*/ "removeobject",
    /*0x55*/ "addobject",
    /*0x56*/ "addobject",
    /*0x57*/ "setobjectxy",
    /*0x58*/ "showobjectat",
    /*0x59*/ "hideobjectat",
    /*0x5A*/ "faceplayer",
    /*0x5B*/ "turnobject",
    /*0x5C*/ "trainerbattle",
    /*0x5D*/ "dotrainerbattle",
    /*0x5E*/ "gotopostbattlescript",
    /*0x5F*/ "gotobeatenscript",
    /*0x60*/ "checktrainerflag",
    /*0x61*/ "settrainerflag",
    /*0x62*/ "cleartrainerflag",
    /*0x63*/ "setobjectxyperm",
    /*0x64*/ "copyobjectxytoperm",
    /*0x65*/ "setobjectmovementtype",
    /*0x66*/ "waitmessage",
    /*0x67*/ "message",
    /*0x68*/ "closemessage",
    /*0x69*/ "lockall",
    /*0x6A*/ "lock",
    /*0x6B*/ "releaseall",
    /*0x6C*/ "release",
    /*0x6D*/ "waitbuttonpress",
    /*0x6E*/ "yesnobox",
    /*0x6F*/ "multichoice",
    /*0x70*/ "multichoicedefault",
    /*0x71*/ "multichoicegrid",
    /*0x72*/ "drawbox",
    /*0x73*/ "erasebox",
    /*0x74*/ "drawboxtext",
    /*0x75*/ "showmonpic",
    /*0x76*/ "hidemonpic",
    /*0x77*/ "showcontestpainting",
    /*0x78*/ "braillemessage",
    /*0x79*/ "givemon",
    /*0x7A*/ "giveegg",
    /*0x7B*/ "setmonmove",
    /*0x7C*/ "checkpartymove",
    /*0x7D*/ "bufferspeciesname",
    /*0x7E*/ "bufferleadmonspeciesname",
    /*0x7F*/ "bufferpartymonnick",
    /*0x80*/ "bufferitemname",
    /*0x81*/ "bufferdecorationname",
    /*0x82*/ "buffermovename",
    /*0x83*/ "buffernumberstring",
    /*0x84*/ "bufferstdstring",
    /*0x85*/ "bufferstring",
    /*0x86*/ "pokemart",
    /*0x87*/ "pokemartdecoration",
    /*0x88*/ "pokemartdecoration2",
    /*0x89*/ "playslotmachine",
    /*0x8A*/ "setberrytree",
    /*0x8B*/ "choosecontestmon",
    /*0x8C*/ "startcontest",
    /*0x8D*/ "showcontestresults",
    /*0x8E*/ "contestlinktransfer",
    /*0x8F*/ "random",
    /*0x90*/ "addmoney",
    /*0x91*/ "removemoney",
    /*0x92*/ "checkmoney",
    /*0x93*/ "showmoneybox",
    /*0x94*/ "hidemoneybox",
    /*0x95*/ "updatemoneybox",
    /*0x96*/ "getpokenewsactive",
    /*0x97*/ "fadescreen",
    /*0x98*/ "fadescreenspeed",
    /*0x99*/ "setflashlevel",
    /*0x9A*/ "animateflash",
    /*0x9B*/ "messageautoscroll",
    /*0x9C*/ "dofieldeffect",
    /*0x9D*/ "setfieldeffectargument",
    /*0x9E*/ "waitfieldeffect",
    /*0x9F*/ "setrespawn",
    /*0xA0*/ "checkplayergender",
    /*0xA1*/ "playmoncry",
    /*0xA2*/ "setmetatile",
    /*0xA3*/ "resetweather",
    /*0xA4*/ "setweather",
    /*0xA5*/ "doweather",
    /*0xA6*/ "setstepcallback",
    /*0xA7*/ "setmaplayoutindex",
    /*0xA8*/ "setobjectsubpriority",
    /*0xA9*/ "resetobjectsubpriority",
    /*0xAA*/ "createvobject",
    /*0xAB*/ "turnvobject",
    /*0xAC*/ "opendoor",
    /*0xAD*/ "closedoor",
    /*0xAE*/ "waitdooranim",
    /*0xAF*/ "setdooropen",
    /*0xB0*/ "setdoorclosed",
    /*0xB1*/ "addelevmenuitem",
    /*0xB2*/ "showelevmenu",
    /*0xB3*/ "checkcoins",
    /*0xB4*/ "addcoins",
    /*0xB5*/ "removecoins",
    /*0xB6*/ "setwildbattle",
    /*0xB7*/ "dowildbattle",
    /*0xB8*/ "setvaddress",
    /*0xB9*/ "vgoto",
    /*0xBA*/ "vcall",
    /*0xBB*/ "vgoto_if",
    /*0xBC*/ "vcall_if",
    /*0xBD*/ "vmessage",
    /*0xBE*/ "vbuffermessage",
    /*0xBF*/ "vbufferstring",
    /*0xC0*/ "showcoinsbox",
    /*0xC1*/ "hidecoinsbox",
    /*0xC2*/ "updatecoinsbox",
    /*0xC3*/ "incrementgamestat",
    /*0xC4*/ "setescapewarp",
    /*0xC5*/ "waitmoncry",
    /*0xC6*/ "bufferboxname",
    /*0xC7*/ "textcolor",
    /*0xC8*/ "loadhelp",
    /*0xC9*/ "unloadhelp",
    /*0xCA*/ "signmsg",
    /*0xCB*/ "normalmsg",
    /*0xCC*/ "comparehiddenvar",
    /*0xCD*/ "setmodernfatefulencounter",
    /*0xCE*/ "checkmodernfatefulencounter",
    /*0xCF*/ "trywondercardscript",
    /*0xD0*/ "setworldmapflag",
    /*0xD1*/ "warpspinenter",
    /*0xD2*/ "setmonmetlocation",
    /*0xD3*/ "moverotatingtileobjects",
    /*0xD4*/ "turnrotatingtileobjects",
    /*0xD5*/ "initrotatingtilepuzzle",
    /*0xD6*/ "freerotatingtilepuzzle",
    /*0xD7*/ "warpmossdeepgym",
    /*0xD8*/ "selectapproachingtrainer",
    /*0xD9*/ "lockfortrainer",
    /*0xDA*/ "closebraillemessage",
    /*0xDB*/ "messageinstant",
    /*0xDC*/ "fadescreenswapbuffers",
    /*0xDD*/ "buffertrainerclassname",
    /*0xDE*/ "buffertrainername",
    /*0xDF*/ "pokenavcall",
    /*0xE0*/ "warpwhitefade",
    /*0xE1*/ "buffercontestname",
    /*0xE2*/ "bufferitemnameplural",
    /*0xE3 -- missing*/ "???",
    /*0xE4 -- missing*/ "???",
    /*0xE5 -- missing*/ "???",
    /*0xE6 -- missing*/ "???",
    /*0xE7 -- missing*/ "???",
    /*0xE8 -- missing*/ "???",
    /*0xE9 -- missing*/ "???",
    /*0xEA -- missing*/ "???",
    /*0xEB -- missing*/ "???",
    /*0xEC -- missing*/ "???",
    /*0xED -- missing*/ "???",
    /*0xEE -- missing*/ "???",
    /*0xEF -- missing*/ "???",
    /*0xF0 -- missing*/ "???",
    /*0xF1 -- missing*/ "???",
    /*0xF2 -- missing*/ "???",
    /*0xF3 -- missing*/ "???",
    /*0xF4 -- missing*/ "???",
    /*0xF5 -- missing*/ "???",
    /*0xF6 -- missing*/ "???",
    /*0xF7 -- missing*/ "???",
    /*0xF8 -- missing*/ "???",
    /*0xF9 -- missing*/ "???",
    /*0xFA -- missing*/ "???",
    /*0xFB -- missing*/ "???",
    /*0xFC -- missing*/ "???",
    /*0xFD -- missing*/ "???",
    /*0xFE -- missing*/ "???",
    /*0xFF -- missing*/ "???",
];
