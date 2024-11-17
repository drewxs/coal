use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(
    name = "Coal",
    author = "Andrew X. Shah <drew@drewxs.dev>",
    version,
    about = "General purpose programming language"
)]
pub struct Cli {
    #[arg(help = "File to run", required = false)]
    pub file: Option<String>,

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
}
