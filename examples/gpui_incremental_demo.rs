use std::time::{Duration, SystemTime};

use futures_lite::StreamExt;
use gpui::AsyncApp;
use gpui::{
    App, Application, Bounds, Context, FontWeight, MouseButton, MouseUpEvent, Timer, Window,
    WindowBounds, WindowOptions, div, prelude::*, px, rgb, size,
};

const STREAM_SEGMENTS: &[&str] = &[
    "# GPUI Hot Update Demo",
    "This example simulates incremental rendering.",
    "Streaming tasks will periodically append new segments.",
    "You can also click the button to inject updates immediately.",
    "The last message will be highlighted for easy observation.",
];

struct IncrementalDemo {
    chunks: Vec<String>,
    last_update: Option<usize>,
    manual_injections: usize,
}

impl IncrementalDemo {
    fn new(_cx: &mut Context<Self>) -> Self {
        Self {
            chunks: Vec::new(),
            last_update: None,
            manual_injections: 0,
        }
    }

    fn append_chunk(&mut self, chunk: String, index: usize, cx: &mut Context<Self>) {
        self.chunks.push(chunk);
        self.last_update = Some(index);
        cx.notify();
    }

    fn inject_manual(
        &mut self,
        _event: &MouseUpEvent,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        self.manual_injections += 1;
        let message = format!(
            "Manually trigger update #{}, time {:?}",
            self.manual_injections,
            SystemTime::now()
        );
        self.chunks.push(message);
        self.last_update = Some(self.chunks.len() - 1);
        cx.notify();
    }
}

impl Render for IncrementalDemo {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        let delivered = self.chunks.len();
        let total = STREAM_SEGMENTS.len();
        let status_line = if delivered < total {
            format!(
                "{} / {} segments delivered, next: {}",
                delivered, total, STREAM_SEGMENTS[delivered]
            )
        } else {
            format!(
                "{} / {} segments delivered, all preset segments completed",
                delivered, total
            )
        };

        let mut timeline = div()
            .flex()
            .flex_col()
            .gap_2()
            .bg(rgb(0x25262c))
            .rounded_lg()
            .border_1()
            .border_color(rgb(0x3a3c42))
            .p_3();

        for (index, chunk) in self.chunks.iter().enumerate() {
            let is_latest = self.last_update == Some(index);
            let cell = div()
                .flex()
                .flex_col()
                .gap_1()
                .bg(if is_latest {
                    rgb(0x334155)
                } else {
                    rgb(0x1f2126)
                })
                .border_1()
                .border_color(if is_latest {
                    rgb(0x65a30d)
                } else {
                    rgb(0x2f3035)
                })
                .rounded_md()
                .p_2()
                .child(format!("{:02}: {}", index + 1, chunk));
            timeline = timeline.child(cell);
        }

        div()
            .flex()
            .flex_col()
            .gap_3()
            .bg(rgb(0x1b1c20))
            .w(px(520.0))
            .p_4()
            .text_color(rgb(0xf1f5f9))
            .child(
                div()
                    .flex()
                    .flex_col()
                    .gap_1()
                    .child(
                        div()
                            .text_xl()
                            .font_weight(FontWeight::BOLD)
                            .child("GPUI Incremental Rendering Demo"),
                    )
                    .child(div().text_sm().text_color(rgb(0x94a3b8)).child(status_line)),
            )
            .child(
                div()
                    .flex()
                    .gap_2()
                    .child(
                        div()
                            .p_2()
                            .rounded_md()
                            .bg(rgb(0x2563eb))
                            .text_color(rgb(0xffffff))
                            .on_mouse_up(MouseButton::Left, cx.listener(Self::inject_manual))
                            .child(format!(
                                "Manually inject update ({})",
                                self.manual_injections
                            )),
                    )
                    .child(
                        div()
                            .p_2()
                            .rounded_md()
                            .bg(rgb(0x1f2933))
                            .text_sm()
                            .text_color(rgb(0xcbd5f5))
                            .child("Click the button to trigger a Context::notify() redraw immediately"),
                    ),
            )
            .child(timeline)
    }
}

fn main() {
    Application::new().run(|app: &mut App| {
        let options = WindowOptions {
            window_bounds: Some(WindowBounds::Windowed(Bounds::centered(
                None,
                size(px(540.0), px(640.0)),
                app,
            ))),
            ..Default::default()
        };

        let demo = app
            .open_window(options, |_, cx| cx.new(IncrementalDemo::new))
            .unwrap();
        let demo_handle = demo;

        let _ = app.spawn(move |cx: &mut AsyncApp| {
            let async_app = cx.clone();
            let demo_handle = demo_handle.clone();

            async move {
                let mut timer = Timer::interval(Duration::from_millis(900));

                for (index, segment) in STREAM_SEGMENTS.iter().enumerate() {
                    if timer.next().await.is_none() {
                        break;
                    }

                    let chunk = (*segment).to_owned();
                    let _ = async_app.update(|app| {
                        let chunk = chunk.clone();
                        let _ = demo_handle.update(app, move |demo, _window, cx| {
                            demo.append_chunk(chunk, index, cx);
                        });
                    });
                }
            }
        });

        app.activate(true);
    });
}
