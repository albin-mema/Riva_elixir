// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html"
// Import CSS
import "../css/app.css"
// Establish Phoenix Socket and LiveView configuration.
import {Socket} from "phoenix"
import {LiveSocket} from "phoenix_live_view"
import topbar from "topbar"

// Import React and ReactDOM for live_react
import React from "react"
import ReactDOM from "react-dom/client"

// Import React components
import "./components"

// Make React available globally for live_react
window.React = React
window.ReactDOM = ReactDOM

// LiveReact hook for React component integration
const LiveReact = {
  mounted() {
    const props = JSON.parse(this.el.dataset.liveReactProps || '{}')
    const componentClass = this.el.dataset.liveReactClass

    if (window.Components && window.Components[componentClass]) {
      const Component = window.Components[componentClass]

      // Create React element with props and pushEvent function
      const element = React.createElement(Component, {
        ...props,
        pushEvent: (event, payload) => this.pushEvent(event, payload)
      })

      // Create root and render
      this.root = ReactDOM.createRoot(this.el)
      this.root.render(element)
    } else {
      console.error(`React component ${componentClass} not found`)
    }
  },

  updated() {
    if (this.root) {
      const props = JSON.parse(this.el.dataset.liveReactProps || '{}')
      const componentClass = this.el.dataset.liveReactClass

      if (window.Components && window.Components[componentClass]) {
        const Component = window.Components[componentClass]

        const element = React.createElement(Component, {
          ...props,
          pushEvent: (event, payload) => this.pushEvent(event, payload)
        })

        this.root.render(element)
      }
    }
  },

  destroyed() {
    if (this.root) {
      this.root.unmount()
    }
  }
}

let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content")
let liveSocket = new LiveSocket("/live", Socket, {
  params: {_csrf_token: csrfToken},
  hooks: { LiveReact }
})

// Show progress bar on live navigation and form submits
topbar.config({barColors: {0: "#29d"}, shadowColor: "rgba(0, 0, 0, .3)"})
window.addEventListener("phx:page-loading-start", _info => topbar.show(300))
window.addEventListener("phx:page-loading-stop", _info => topbar.hide())

// connect if there are any LiveViews on the page
liveSocket.connect()

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket
