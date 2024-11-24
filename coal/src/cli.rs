use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(
    name = "Coal",
    author = "Andrew X. Shah <drew@drewxs.dev>",
    version,
    about = "Run without arguments to start the REPL"
)]
pub struct Cli {
    #[arg(help = "File to run", required = false)]
    pub file: Option<String>,

    #[arg(short = 'e', long)]
    #[arg(help = "Evaluate script")]
    pub eval: Option<String>,

    #[command(subcommand)]
    pub cmd: Option<Command>,
}

#[derive(Subcommand)]
pub enum Command {
    #[command(about = "Format files")]
    Fmt {
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
