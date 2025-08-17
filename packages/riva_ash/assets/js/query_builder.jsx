import React, { useMemo, useState } from "react";
import { Query, Builder, Utils as QbUtils } from "@react-awesome-query-builder/ui";
import "@react-awesome-query-builder/ui/css/styles.css";
// Use MUI kit (shadcn is not directly supported by RAQB)
import { MuiConfig as MUIConfig } from "@react-awesome-query-builder/mui";
import "@react-awesome-query-builder/mui/css/styles.css";
import { ThemeProvider, createTheme } from "@mui/material/styles";

// Minimal wrapper used by LiveReact. We attach to window.QueryBuilder
function QueryBuilder(props) {
  const { config = {}, value = null, onChangeEventName = null, useTokens = false } = props || {};
  const [tree, setTree] = useState(value || QbUtils.loadTree({ id: QbUtils.uuid(), type: "group" }));

  const qbConfig = useMemo(() => {
    const defaultFields = {
      qty: { label: "Quantity", type: "number" },
      price: { label: "Price", type: "number" },
      name: { label: "Name", type: "text" },
      date: { label: "Date", type: "date" }
    };
    const providedFields = (config && config.fields) || {};
    const fields = Object.keys(providedFields).length ? providedFields : defaultFields;
    return {
      ...MUIConfig,
      ...config,
      fields,
    };
  }, [config]);

  const onChange = (immutableTree, _config) => {
    setTree(immutableTree);
    try {
      const json = QbUtils.getTree(immutableTree);
      // Prefer LiveView hook bridge if available
      if (typeof props?.livePush === "function") {
        props.livePush("qb_changed", { tree: json });
      } else if (onChangeEventName && window.liveSocket) {
        // Fallback legacy custom event
        const event = new CustomEvent(onChangeEventName, { detail: { tree: json } });
        window.dispatchEvent(event);
      }
    } catch (e) {
      console.error("QueryBuilder onChange dispatch failed", e);
    }
  };

  // Best-effort MUI theme: optionally derive from CSS tokens
  const theme = useMemo(() => {
    if (!useTokens) return createTheme({});
    const root = document.documentElement;
    const get = (name, fallback) => {
      const v = getComputedStyle(root).getPropertyValue(name).trim();
      return v || fallback;
    };
    return createTheme({
      palette: {
        primary: { main: get("--primary", undefined) },
        secondary: { main: get("--secondary", undefined) },
        error: { main: get("--destructive", undefined) },
        background: { default: get("--background", undefined) },
        text: { primary: get("--foreground", undefined) },
      },
    });
  }, [useTokens]);

  return (
    <ThemeProvider theme={theme}>
      <div data-testid="react-query-builder" className="w-full">
        <Query value={tree} onChange={onChange} config={qbConfig}>
          {({ builders, onPropsChanged, ...rest }) => (
            <div className="qb-lite">
              <Builder {...rest} onPropsChanged={onPropsChanged} builders={builders} />
            </div>
          )}
        </Query>
      </div>
    </ThemeProvider>
  );
}

// Expose globally for LiveReact hook
window.QueryBuilder = QueryBuilder;
export default QueryBuilder;

