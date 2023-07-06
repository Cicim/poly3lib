use colored::Colorize;

use crate::{rom::Rom, text::Text};

use super::{consts::SCRIPT_NAMES, tree::visit_script, ScriptResource, ScriptTree};

// Copy write!(f, "fmt", ...)
macro_rules! output {
    ($res:expr, $fmt:expr) => {
        $res.add(&format!($fmt));
    };
    ($res:expr, $fmt:expr, $($arg:tt)*) => {
        $res.add(&format!($fmt, $($arg)*));
    };
}

impl ScriptTree {
    pub fn print(&self, rom: &Rom, color: bool) {
        let mut res = OutputBuffer::new(color);

        // Loop through all the resources
        for (resource, _) in self.map.iter() {
            resource.print(&mut res, rom);
            output!(res, "\n");
        }

        println!("{}", res.string);
    }
}

impl ScriptResource {
    fn print(&self, res: &mut OutputBuffer, rom: &Rom) {
        // Write the header
        res.start_row(Some(self.offset() as usize));
        output!(
            res,
            "{} {}\n",
            res.keyword(".org"),
            res.format_as_offset(self)
        );

        match self {
            ScriptResource::Script(offset) => {
                let offset = *offset as usize;

                match visit_script(rom, offset, |code, args| Some((code, args.to_vec()))) {
                    Ok(script) => {
                        res.add_script(offset, script);
                    }
                    Err(_) => {
                        res.start_row(Some(offset));
                        res.error("Could not read script");
                    }
                };
            }
            ScriptResource::Text(offset) => {
                let offset = *offset as usize;
                // Read the text
                match rom.read_text(offset) {
                    Ok(text) => {
                        res.add_text(text);
                    }
                    Err(_) => {
                        res.error("Failed to read text");
                        return;
                    }
                }
            }
            ScriptResource::Movement(offset) => {
                let mut offset = *offset as usize;

                // Write the products
                while let Some(movement) = res.add_movement(rom, offset) {
                    offset += 1;
                    if movement == 0xFE {
                        break;
                    }
                }
            }
            ScriptResource::Products(offset) => {
                let mut offset = *offset as usize;

                // Write the products
                while let Some(product) = res.add_product(rom, offset) {
                    offset += 2;
                    if product == 0 {
                        break;
                    }
                }

                // Write the last commands
                res.add_products_release_end(rom, offset);
            }
            ScriptResource::InvalidPointer(offset) => {
                let offset = *offset as usize;
                res.start_row(Some(offset));
                res.error("Invalid pointer");
            }
        }
    }
}

struct OutputBuffer {
    string: String,
    color: bool,
    show_offset: bool,
}

impl OutputBuffer {
    fn new(color: bool) -> Self {
        Self {
            string: String::new(),
            color,
            show_offset: true,
        }
    }

    fn add(&mut self, string: &str) {
        self.string.push_str(string);
    }

    fn error(&mut self, string: &str) {
        let res = format!("  ERROR: {}\n", string);
        if self.color {
            self.add(&res.bright_red().to_string());
        } else {
            self.add(&res);
        }
    }

    fn start_row(&mut self, offset: Option<usize>) {
        if !self.show_offset {
            return;
        }

        if let Some(offset) = offset {
            let str = format!("{:>8X}>", offset);
            if self.color {
                self.add(&str.white().to_string());
            } else {
                self.add(&str);
            }
            return;
        }

        if self.show_offset {
            if self.color {
                self.add(&"        >".white().to_string())
            } else {
                self.add("        >");
            }
        }
    }

    fn keyword(&self, keyword: &str) -> String {
        if self.color {
            keyword.red().to_string()
        } else {
            keyword.to_string()
        }
    }

    fn format_as_offset(&self, res: &ScriptResource) -> String {
        use ScriptResource::*;

        let prefix = match res {
            Script(_) => "script",
            Text(_) => "text",
            Movement(_) => "movement",
            Products(_) => "products",
            InvalidPointer(_) => panic!("You should never need to format an InvalidPointer"),
        };

        let str = format!("@{}_{:0x}", prefix, res.offset());
        if self.color {
            str.cyan().to_string()
        } else {
            str
        }
    }

    fn cmd(&self, cmd: &str) -> String {
        let str = format!("{}", cmd);
        if self.color {
            str.green().to_string()
        } else {
            str
        }
    }

    fn format_as_byte(&self, value: u8) -> String {
        let str = format!("0x{:02x}", value);
        if self.color {
            str.yellow().to_string()
        } else {
            str
        }
    }

    fn format_as_2bytes(&self, value: u16) -> String {
        let str = format!("0x{:04x}", value);
        if self.color {
            str.yellow().to_string()
        } else {
            str
        }
    }

    fn add_product(&mut self, rom: &Rom, offset: usize) -> Option<u16> {
        match rom.read_unaligned_halfword(offset) {
            Ok(value) => {
                self.start_row(Some(offset));

                output!(
                    self,
                    "  {} {}\n",
                    self.keyword(".2bytes"),
                    self.format_as_2bytes(value)
                );
                Some(value)
            }
            Err(_) => None,
        }
    }

    fn add_products_release_end(&mut self, rom: &Rom, offset: usize) {
        self.start_row(Some(offset));

        let should_be_release = rom.read_byte(offset);
        if should_be_release != 0x6C {
            self.error("Expected release command");
            return;
        }
        self.add(&format!("  {}\n", self.cmd("release")));

        self.start_row(Some(offset + 1));
        let should_be_end = rom.read_byte(offset + 1);
        if should_be_end != 0x02 {
            self.error("Expected end command");
            return;
        }
        self.add(&format!("  {}\n", self.cmd("end")));
    }

    fn add_movement(&mut self, rom: &Rom, offset: usize) -> Option<u8> {
        match rom.read::<u8>(offset) {
            Ok(value) => {
                self.start_row(Some(offset));

                output!(
                    self,
                    "  {} {}\n",
                    self.keyword(".byte"),
                    self.format_as_byte(value)
                );
                Some(value)
            }
            Err(_) => None,
        }
    }

    fn add_text(&mut self, text: Text) {
        // Split the text by newlines
        let split = text.split_by_newline();

        for line in split {
            self.start_row(None);
            // Get the string of text
            let string = format!("\"{}\"", Text::debug_format(line));
            let string = if self.color {
                string.yellow().to_string()
            } else {
                string
            };

            // Print it
            self.add(&format!("  {} {}\n", self.keyword(".string"), string));
        }
    }

    fn add_script(&mut self, mut offset: usize, script: Vec<(u8, Vec<u8>)>) {
        for (code, bytes) in script {
            // Print the instruction
            let name = self.cmd(SCRIPT_NAMES[code as usize]);

            // Print the bytes
            let mut bytes_str = String::new();
            for byte in &bytes {
                bytes_str.push_str(&format!("0x{:02x} ", byte));
            }

            self.start_row(Some(offset));
            self.add(&format!("  {} {}\n", name, bytes_str));

            offset += bytes.len() + 1;
        }
    }
}
