use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(
    name = "Coal",
    author = "Andrew X. Shah <drew@drewxs.dev>",
    version,
    about = "Run without arguments to start the REPL"
)]
pub struct Cli {
    #[command(subcommand)]
    pub cmd: Option<Command>,
}

#[derive(Subcommand)]
pub enum Command {
    #[command(about = "Create a new project")]
    New { name: String },

    #[command(about = "Compile the current project")]
    #[command(visible_alias = "c")]
    Compile,

    #[command(about = "Run the current project")]
    #[command(visible_alias = "r")]
    Run,

    #[command(about = "Execute a standalone script")]
    #[command(visible_alias = "x")]
    Exec { path: String },

    #[command(about = "Parse a given program")]
    #[command(visible_alias = "p")]
    Parse {
        path: String,

        #[arg(short = 't', long, action)]
        tokens: bool,

        #[arg(short = 'a', long, action)]
        ast: bool,
    },

    #[command(about = "Check for parsing errors")]
    #[command(visible_alias = "l")]
    Lint {
        input: Option<String>,

        #[arg(short = 'p', long, help = "Lint a given path")]
        path: Option<String>,
    },

    #[command(about = "Format files")]
    #[command(long_about = "Format all files in the current project or from a given path")]
    #[command(visible_alias = "f")]
    Fmt {
        input: Option<String>,

        #[arg(short = 'p', long, help = "Format a given path")]
        path: Option<String>,

        #[arg(short = 'n', long)]
        #[arg(help = "Print formatted code to stdout")]
        dry_run: bool,
    },

    #[command(about = "Data management")]
    Data {
        #[command(subcommand)]
        cmd: DataCommand,
    },

    #[command(about = "Cache management")]
    Cache {
        #[command(subcommand)]
        cmd: CacheCommand,
    },
}

#[derive(Subcommand)]
pub enum DataCommand {
    #[command(about = "Clear the data directory")]
    Clear,
}

#[derive(Subcommand)]
pub enum CacheCommand {
    #[command(about = "Clear the cache directory")]
    Clear,
}
