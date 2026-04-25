use std::{fs, io, process::Command};

const HELLO_TEMPLATE: &str = include_str!("../../templates/hello.coal");

pub fn new(name: &str) -> io::Result<String> {
    fs::create_dir_all(format!("{name}/src"))?;
    fs::write(format!("{name}/src/main.coal"), HELLO_TEMPLATE)?;
    fs::write(format!("{name}/README.md"), format!("# {name}\n"))?;
    fs::write(format!("{name}/.gitignore"), "/target\n")?;

    Command::new("git").arg("init").arg(name).output()?;

    Ok(format!("Project created `{name}`"))
}
