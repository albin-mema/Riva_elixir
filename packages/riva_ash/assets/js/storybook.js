// Storybook specific JavaScript
// This file is used for Phoenix Storybook integration

// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html"
// Establish Phoenix Socket and LiveView configuration.
import {Socket} from "phoenix"
import {LiveSocket} from "phoenix_live_view"
import { LiveReact } from "./live_react_hook"
import "./query_builder.jsx"
import "./react_hello.jsx"
import "./simple_react.jsx"
// Ensure React is available globally for LiveReact renderers in some addons
import React from "react";
import ReactDOM from "react-dom/client";
window.React = React;
window.ReactDOM = ReactDOM;

let csrfToken = document.querySelector("meta[name='csrf-token']")?.getAttribute("content")
let liveSocket = new LiveSocket("/live", Socket, {params: {_csrf_token: csrfToken}, hooks: { LiveReact }})

// connect if there are any LiveViews on the page
liveSocket.connect()

// expose liveSocket on window for web console debug logs
window.liveSocket = liveSocket
