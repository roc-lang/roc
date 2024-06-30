use core::{
    cell::{RefCell, RefMut},
    ops::DerefMut,
};

mod multi_threaded_worker;
mod single_threaded_worker;
mod worker;

// pub trait JobRunner {
//     type Event;
//     type State;
//     type IoRequests;

//     fn update(&self, evt: Self::Event, state: &mut Self::State) -> Self::IoRequests;
//     fn process_io(&self, requests: Self::IoRequests) -> Self::IoRequests;
// }

// // pub struct SingleThreadedRunner<State> {
// //     state: RefCell<State>,
// // }

// // impl<'a, S> JobRunner<RefMut<'a, S>> for SingleThreadedRunner<S> {
// //     type State = S;

// //     fn state(&'a self) -> Option<RefMut<'a, S>> {
// //         Some(self.state.borrow_mut())
// //     }

// //     fn into_state(self) -> Option<Self::State> {
// //         todo!()
// //     }
// // }

// // use std::sync::{Arc, Mutex, MutexGuard, PoisonError};

// // pub struct MultithreadedRunner<State> {
// //     state: Arc<Mutex<State>>,
// // }

// // impl<'a, S> JobRunner<MutexGuard<'a, S>> for MultithreadedRunner<S> {
// //     type State = S;

// //     fn state(&self) -> Option<MutexGuard<'a, S>> {
// //         self.state.try_lock().ok()
// //     }

// //     fn into_state(self) -> Option<Self::State> {
// //         todo!()
// //     }
// // }
