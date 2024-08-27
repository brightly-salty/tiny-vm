extern crate vergen;
use anyhow::Result;
use vergen::EmitBuilder;

fn main() -> Result<()> {
    embed_resource::compile("manifest.rc", embed_resource::NONE);
    EmitBuilder::builder()
        .build_date()
        .git_describe(true, false, None)
        .emit()?;

    Ok(())
}
