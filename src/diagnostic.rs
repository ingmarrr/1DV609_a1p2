pub trait Diagnostic: Clone {
    fn report(&mut self, info: DiagnosticInfo);
}

pub struct DiagnosticInfo {
    pub message: String,
}

impl<'a> Diagnostic for std::rc::Rc<std::cell::RefCell<Vec<DiagnosticInfo>>> {
    fn report(&mut self, info: DiagnosticInfo) {
        self.borrow_mut().push(info.into());
    }
}

impl Diagnostic for () {
    fn report(&mut self, _: DiagnosticInfo) {}
}
