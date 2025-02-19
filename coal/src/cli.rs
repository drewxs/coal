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

    #[command(about = "Compile and run programs")]
    #[command(long_about = "Compile and run the current project or a given program")]
    #[command(visible_alias = "r")]
    Run {
        path: Option<String>,

        #[arg(short = 't', long, action)]
        tokens: bool,

        #[arg(short = 'a', long, action)]
        ast: bool,
    },

    #[command(about = "Evaluate scripts")]
    #[command(visible_alias = "e")]
    Eval {
        input: String,

        #[arg(short = 't', long, action)]
        tokens: bool,

        #[arg(short = 'a', long, action)]
        ast: bool,
    },

    #[command(about = "Check for parsing errors")]
    Lint {
        input: Option<String>,

        #[arg(short = 'p', long, help = "Lint a given path")]
        path: Option<String>,
    },

    #[command(about = "Format files")]
    #[command(long_about = "Format all files in the current project or from a given path")]
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
