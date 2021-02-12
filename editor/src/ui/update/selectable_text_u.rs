impl SelectableText {
    pub fn move_caret_w_mods(&mut self, new_pos: Position, mods: &ModifiersState) {
        let caret_pos = self.caret_pos;

        // one does not simply move the caret
        if new_pos != caret_pos {
            if mods.shift() {
                if let Some(selection) = self.selection_opt {
                    if new_pos < selection.start_pos {
                        if caret_pos > selection.start_pos {
                            self.set_selection(
                                new_pos,
                                selection.start_pos
                            )
                        } else {
                            self.set_selection(
                                new_pos,
                                selection.end_pos
                            )
                        }
                    } else if new_pos > selection.end_pos {
                        if caret_pos < selection.end_pos {
                            self.set_selection(
                                selection.end_pos,
                                new_pos
                            )
                        } else {
                            self.set_selection(
                                selection.start_pos,
                                new_pos
                            )
                        }
                    } else if new_pos > caret_pos {
                        self.set_selection(
                            new_pos,
                            selection.end_pos
                        )
                    } else if new_pos < caret_pos {
                        self.set_selection(
                            selection.start_pos,
                            new_pos
                        )
                    }
                } else if new_pos < self.caret_pos {
                        self.set_selection(
                            new_pos,
                            caret_pos
                        ) 
                } else {
                    self.set_selection(
                        caret_pos,
                        new_pos
                    ) 
                }
            } else {
                self.selection_opt = None;
            }

            self.caret_pos = new_pos;
        }
    }
}