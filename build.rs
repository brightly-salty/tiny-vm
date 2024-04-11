extern crate vergen;
use anyhow::Result;
use vergen::EmitBuilder;

fn main() -> Result<()> {
    EmitBuilder::builder()
        .build_date()
        .git_describe(true, false, None)
        .emit()?;

    Ok(())
}
