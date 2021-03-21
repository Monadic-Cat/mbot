//! Graphing distributions for dice expressions.
//! I didn't feel like doing much math for this one,
//! so, as a preliminary attempt, we just take a ton
//! of samples and chart them.
//! Because of this, we should consider limiting the
//! size of input dice expressions more harshly.

use ::plotters::prelude::*;
use ::plotters_bitmap::BitMapBackend;
use ::plotters_bitmap::bitmap_pixel::{RGBPixel, PixelFormat};
use ::plotters::data::fitting_range;
use ::mice::parse::Expression;
use ::mice::prelude::MiceError;
use ::core::iter;
use ::std::collections::HashMap;

const RESOLUTION: (u32, u32) = (600, 600);
const BUFFER_WIDTH: usize = (RESOLUTION.0 * RESOLUTION.1) as usize * <RGBPixel as PixelFormat>::PIXEL_SIZE;
const SAMPLE_SIZE: usize = 1_000_000;

// TODO: adjust sampling for larger inputs?
pub(crate) fn draw(exp: &Expression, caption: &str) -> Result<Box<[u8]>, MiceError> {
    // This is FAR too expensive computationally right now.
    let mut counts = HashMap::new();
    let mut rng = {
        use ::rand::SeedableRng;
        ::rand::rngs::SmallRng::from_entropy()
    };
    let sample = iter::repeat_with(|| exp.roll_with(&mut rng).map(|x| x.total()))
        .inspect(|x| x.iter().for_each(|x| *counts.entry(*x).or_insert(0) += 1))
        .take(SAMPLE_SIZE).collect::<Result<Vec<_>, _>>()?;
    let max = *counts.values().max().unwrap() * 4 / 3;
    // Note: We could *definitely* reuse this buffer.
    let mut buffer = vec!(0u8; BUFFER_WIDTH);
    // ## Plot a Histogram
    {
        let root = BitMapBackend::with_buffer(&mut buffer, RESOLUTION);
        let area = root.into_drawing_area();
        area.fill(&WHITE).unwrap();
        let mut chart = ChartBuilder::on(&area)
            .caption(caption, ("sans-serif", 50).into_font())
            .margin(5)
            .x_label_area_size(30)
            .y_label_area_size(30)
            .build_cartesian_2d({
                let mut domain = fitting_range(&sample);
                domain.end += 1;
                domain
            }, 0i64..max).unwrap();
        chart.configure_mesh().y_label_formatter(&|y| format!("{:.0}%", (*y as f64) * 100.0 / SAMPLE_SIZE as f64)).draw().unwrap();
        chart.draw_series(Histogram::vertical(&chart)
                          .style(RED.mix(0.5).filled())
                          .data(sample.iter().map(|x| (*x as i32, 1)))).unwrap();
    }
    // ## Encode as PNG
    let mut image = Vec::new();
    {
        let mut encoder = ::png::Encoder::new(&mut image, RESOLUTION.0, RESOLUTION.1);
        encoder.set_color(::png::ColorType::RGB);
        encoder.set_depth(::png::BitDepth::Eight);
        let mut writer = encoder.write_header().unwrap();
        writer.write_image_data(&buffer).unwrap();
    }
    Ok(image.into_boxed_slice())
}
