use image::RgbaImage;

use super::{graphics::ObjectEventData, ObjectEventError};

impl ObjectEventData {
    /// Renders the object event data at the start of the frame.
    pub fn render_start(&self, anim_id: u8) -> Result<RgbaImage, ObjectEventError> {
        // Get the animation data.
        let anim_data = self
            .anims
            .get(anim_id as usize)
            .ok_or(ObjectEventError::InvalidAnimId(anim_id))?;

        // Get the animation frame data.
        let frame = anim_data.get_first_frame()?;

        // Get the image from the loaded ones
        let graphics = self
            .images
            .get(frame.image_value as usize)
            .ok_or(ObjectEventError::InvalidImageId(frame.image_value))?;

        // Create a new rgba image
        let mut res_image: RgbaImage = RgbaImage::new(self.width, self.height);

        // Draw the tile with the given transformations onto that image
        graphics.draw(
            &mut res_image,
            &self.palette,
            frame.hflip != 0,
            frame.vflip != 0,
        );

        Ok(res_image)
    }
}
