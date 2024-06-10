#shellcheck shell=sh
redo-ifchange Cargo.toml Cargo.lock
profile=$(printf '%s\n' "$2" | sed 's,target/\([^/]*\)/font_encoder,\1,;t;d')
cargo build --profile "$profile"
mv "$2" "$3"
cut -d : -f 2- <"$2.d" | xargs redo-ifchange
