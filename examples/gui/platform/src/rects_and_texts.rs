
use crate::graphics::primitives::rect::Rect;
use crate::graphics::primitives::text::{owned_section_from_text, Text};

#[derive(Debug)]
pub struct RectsAndTexts {
    pub text_sections_behind: Vec<glyph_brush::OwnedSection>, // displayed in front of rect_behind, behind everything else
    pub text_sections_front: Vec<glyph_brush::OwnedSection>,  // displayed in front of everything
    pub rects_behind: Vec<Rect>,                              // displayed at lowest depth
    pub rects_front: Vec<Rect>, // displayed in front of text_sections_behind, behind text_sections_front
}

impl RectsAndTexts {
    pub fn new() -> Self {
        Self {
            text_sections_behind: Vec::new(),
            text_sections_front: Vec::new(),
            rects_behind: Vec::new(),
            rects_front: Vec::new(),
        }
    }

    pub fn add_text_behind(&mut self, new_text_section: glyph_brush::OwnedSection) {
        self.text_sections_behind.push(new_text_section);
    }

    pub fn add_text_front(&mut self, new_text_section: glyph_brush::OwnedSection) {
        self.text_sections_front.push(new_text_section);
    }

    pub fn add_rect_behind(&mut self, new_rect: Rect) {
        self.rects_behind.push(new_rect);
    }

    pub fn add_rects_behind(&mut self, new_rects: Vec<Rect>) {
        self.rects_behind.extend(new_rects);
    }

    pub fn add_rect_front(&mut self, new_rect: Rect) {
        self.rects_front.push(new_rect);
    }

    pub fn extend(&mut self, rects_and_texts: RectsAndTexts) {
        self.text_sections_behind
            .extend(rects_and_texts.text_sections_behind);
        self.text_sections_front
            .extend(rects_and_texts.text_sections_front);
        self.rects_behind.extend(rects_and_texts.rects_behind);
        self.rects_front.extend(rects_and_texts.rects_front);
    }
}