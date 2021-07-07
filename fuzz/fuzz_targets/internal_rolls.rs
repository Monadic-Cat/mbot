#![no_main]
use libfuzzer_sys::fuzz_target;

use ::mbot::fuzz::internal_rolls::response_for;

fuzz_target!(|data: &[u8]| {
    // Length of a Discord message is limited to 2k.
    if data.len() <= 2000 {
        // Discord messages are guaranteed valid UTF-8.
        if let Ok(msg) = ::core::str::from_utf8(data) {
            let _ = response_for(msg);
        }
    }
});
