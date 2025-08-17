// Phoenix LiveView hook to mount React components via live_react attributes
// Expects:
//  - data-live-react-class: global component name on window (e.g., "QueryBuilder")
//  - data-live-react-props: JSON string with props

import React from "react";
import ReactDOM from "react-dom/client";

export const LiveReact = {
  mounted() {
    this.mountReact();
  },
  updated() {
    this.mountReact();
  },
  destroyed() {
    this.unmountReact();
  },
  mountReact() {
    const compName = this.el.dataset.liveReactClass;
    const propsJson = this.el.dataset.liveReactProps || "{}";
    const props = safeParse(propsJson);

    if (!compName) {
      console.warn("LiveReact: missing data-live-react-class on", this.el);
      return;
    }

    const Comp = window[compName];
    if (!Comp) {
      console.error(`LiveReact: component ${compName} not found on window`);
      return;
    }

    // Bridge callback to LiveView
    const bridgedProps = {
      ...props,
      livePush: (event, payload) => this.pushEvent(event, payload || {}),
    };

    // reuse root across updates
    if (!this._reactRoot) {
      this._reactRoot = ReactDOM.createRoot(this.el);
    }
    this._reactRoot.render(React.createElement(Comp, bridgedProps));
  },
  unmountReact() {
    if (this._reactRoot) {
      this._reactRoot.unmount();
      this._reactRoot = null;
    }
  },
};

function safeParse(json) {
  try {
    return JSON.parse(json);
  } catch (e) {
    console.error("LiveReact: failed to parse props JSON", e);
    return {};
  }
}

