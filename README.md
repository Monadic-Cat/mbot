[![Build Status](https://travis-ci.org/Monadic-Cat/mbot.svg?branch=master)](https://travis-ci.org/Monadic-Cat/mbot)

# mbot, mice bot I guess
This is a discord bot for personal usage- it will accrue features
as I want them.

# Building
This project builds like any Cargo binary crate.
Use the Cargo build commands.

# Running
Set the environment variable `MBOT_TOKEN` to the bot token you're using,
then run it like any Cargo binary.

You may also set the `INSTANCE_MESSAGE_PREFIX` environment variable,
which will add the given prefix to all replies the bot sends.
I may change that variable name sometime,
probably to something like `MBOT_INSTANCE_MESSAGE_PREFIX`.

# Project Structure
`cmice` - A crude CLI tool using the same dice library as the bot, for manual testing sometimes
`data` - Just contains a vendored JSON file pulled from another bot project that's focused on a particular game I play
`mice` - The dice library used by the bot and CLI.
`proc_macro_helpers` - A proc macro crate where I stick whatever proc macros I write for the project (currently just a helper for declaring operators together with their binding powers)
`reloadable-plotter` - A histogram plotter for dice expressions
`slash` - A WIP slash commands server that doesn't work anymore lol
`src` - The main source code for the bot, but which mostly just delegates to `mice` and `reloadable-plotter` for the big things, at the moment.
