/**
 * Enhanced Mermaid Hook for Interactive Diagrams
 * 
 * Features:
 * - Real-time diagram rendering
 * - Interactive node clicking
 * - Zoom and pan controls
 * - Export functionality
 * - Theme switching
 */

import mermaid from 'mermaid';

// Initialize Mermaid with enhanced configuration
mermaid.initialize({
  startOnLoad: false,
  theme: 'default',
  themeVariables: {
    primaryColor: '#3B82F6',
    primaryTextColor: '#1F2937',
    primaryBorderColor: '#2563EB',
    lineColor: '#6B7280',
    secondaryColor: '#F3F4F6',
    tertiaryColor: '#E5E7EB',
    background: '#FFFFFF',
    mainBkg: '#FFFFFF',
    secondBkg: '#F9FAFB',
    tertiaryBkg: '#F3F4F6'
  },
  flowchart: {
    useMaxWidth: true,
    htmlLabels: true,
    curve: 'basis'
  },
  sequence: {
    diagramMarginX: 50,
    diagramMarginY: 10,
    actorMargin: 50,
    width: 150,
    height: 65,
    boxMargin: 10,
    boxTextMargin: 5,
    noteMargin: 10,
    messageMargin: 35,
    mirrorActors: true,
    bottomMarginAdj: 1,
    useMaxWidth: true,
    rightAngles: false,
    showSequenceNumbers: false
  },
  gantt: {
    titleTopMargin: 25,
    barHeight: 20,
    fontFamily: '"Inter", "system-ui", sans-serif',
    fontSize: 11,
    fontWeight: 'normal',
    gridLineStartPadding: 35,
    leftPadding: 75,
    topPadding: 50,
    rightPadding: 75,
    bottomPadding: 25
  }
});

export const MermaidHook = {
  mounted() {
    this.renderDiagram();
    this.setupInteractivity();
    this.setupControls();
  },

  updated() {
    this.renderDiagram();
  },

  destroyed() {
    this.cleanup();
  },

  renderDiagram() {
    const diagramDefinition = this.el.dataset.diagram;
    const diagramId = `mermaid-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    
    if (!diagramDefinition) {
      console.warn('No diagram definition found');
      return;
    }

    // Clear previous content
    this.el.innerHTML = '';

    // Create container for diagram
    const container = document.createElement('div');
    container.className = 'mermaid-container relative';
    container.innerHTML = `
      <div class="mermaid-controls absolute top-2 right-2 z-10 flex space-x-2">
        <button class="toggle-view bg-blue-600 text-white shadow-md rounded p-2 hover:bg-blue-700" title="Toggle Chart/Code View">
          <svg class="w-4 h-4 chart-icon" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2-2V7a2 2 0 012-2h2a2 2 0 002 2v2a2 2 0 002 2h2a2 2 0 002-2V7a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 00-2 2h-2a2 2 0 00-2 2v6a2 2 0 01-2 2H9z"></path>
          </svg>
          <svg class="w-4 h-4 code-icon hidden" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4"></path>
          </svg>
        </button>
        <button class="zoom-in bg-white shadow-md rounded p-2 hover:bg-gray-50" title="Zoom In">
          <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6"></path>
          </svg>
        </button>
        <button class="zoom-out bg-white shadow-md rounded p-2 hover:bg-gray-50" title="Zoom Out">
          <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M18 12H6"></path>
          </svg>
        </button>
        <button class="reset-zoom bg-white shadow-md rounded p-2 hover:bg-gray-50" title="Reset Zoom">
          <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"></path>
          </svg>
        </button>
        <button class="export-svg bg-white shadow-md rounded p-2 hover:bg-gray-50" title="Export SVG">
          <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 10v6m0 0l-3-3m3 3l3-3m2 8H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"></path>
          </svg>
        </button>
        <button class="toggle-theme bg-white shadow-md rounded p-2 hover:bg-gray-50" title="Toggle Theme">
          <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z"></path>
          </svg>
        </button>
      </div>
      <div class="mermaid-diagram-wrapper overflow-auto border border-gray-200 rounded-lg bg-white" style="height: 500px;">
        <div id="${diagramId}" class="mermaid-diagram p-4"></div>
      </div>
      <div class="mermaid-code-wrapper overflow-auto border border-gray-200 rounded-lg bg-gray-900 text-green-400 font-mono text-sm hidden" style="height: 500px;">
        <div class="p-4">
          <pre class="whitespace-pre-wrap">${this.escapeHtml(diagramDefinition)}</pre>
        </div>
      </div>
      <div class="mermaid-info mt-2 text-sm text-gray-600">
        <span class="diagram-stats"></span>
        <span class="float-right">Click nodes for details • Scroll to zoom • Drag to pan</span>
      </div>
    `;

    this.el.appendChild(container);

    // Render the diagram
    try {
      mermaid.render(diagramId, diagramDefinition).then(({ svg, bindFunctions }) => {
        const diagramElement = document.getElementById(diagramId);
        diagramElement.innerHTML = svg;
        
        // Bind any interactive functions
        if (bindFunctions) {
          bindFunctions(diagramElement);
        }

        // Setup zoom and pan
        this.setupZoomPan(diagramElement);
        
        // Setup node interactions
        this.setupNodeInteractions(diagramElement);
        
        // Update stats
        this.updateDiagramStats(diagramElement);
        
        // Notify LiveView that diagram is ready
        this.pushEvent('diagram_rendered', { 
          id: diagramId,
          nodeCount: diagramElement.querySelectorAll('.node').length,
          edgeCount: diagramElement.querySelectorAll('.edge').length
        });
      }).catch(error => {
        console.error('Mermaid rendering error:', error);
        this.showError(container, error.message);
      });
    } catch (error) {
      console.error('Mermaid parsing error:', error);
      this.showError(container, error.message);
    }
  },

  setupInteractivity() {
    // Handle node clicks
    this.handleEvent('highlight_node', ({ nodeId }) => {
      this.highlightNode(nodeId);
    });

    this.handleEvent('update_diagram', ({ diagram }) => {
      this.el.dataset.diagram = diagram;
      this.renderDiagram();
    });
  },

  setupControls() {
    this.el.addEventListener('click', (e) => {
      if (e.target.closest('.toggle-view')) {
        this.toggleView();
      } else if (e.target.closest('.zoom-in')) {
        this.zoomIn();
      } else if (e.target.closest('.zoom-out')) {
        this.zoomOut();
      } else if (e.target.closest('.reset-zoom')) {
        this.resetZoom();
      } else if (e.target.closest('.export-svg')) {
        this.exportSVG();
      } else if (e.target.closest('.toggle-theme')) {
        this.toggleTheme();
      }
    });
  },

  setupZoomPan(diagramElement) {
    let scale = 1;
    let translateX = 0;
    let translateY = 0;
    let isDragging = false;
    let lastX = 0;
    let lastY = 0;

    const svg = diagramElement.querySelector('svg');
    if (!svg) return;

    // Store transform state
    this.transform = { scale, translateX, translateY };

    // Zoom with mouse wheel
    diagramElement.addEventListener('wheel', (e) => {
      e.preventDefault();
      const delta = e.deltaY > 0 ? 0.9 : 1.1;
      this.transform.scale *= delta;
      this.transform.scale = Math.max(0.1, Math.min(3, this.transform.scale));
      this.applyTransform(svg);
    });

    // Pan with mouse drag
    diagramElement.addEventListener('mousedown', (e) => {
      if (e.target.closest('.node') || e.target.closest('.edge')) return;
      isDragging = true;
      lastX = e.clientX;
      lastY = e.clientY;
      diagramElement.style.cursor = 'grabbing';
    });

    document.addEventListener('mousemove', (e) => {
      if (!isDragging) return;
      const deltaX = e.clientX - lastX;
      const deltaY = e.clientY - lastY;
      this.transform.translateX += deltaX;
      this.transform.translateY += deltaY;
      this.applyTransform(svg);
      lastX = e.clientX;
      lastY = e.clientY;
    });

    document.addEventListener('mouseup', () => {
      isDragging = false;
      diagramElement.style.cursor = 'default';
    });
  },

  setupNodeInteractions(diagramElement) {
    // Add click handlers to nodes
    diagramElement.querySelectorAll('.node').forEach(node => {
      node.style.cursor = 'pointer';
      node.addEventListener('click', (e) => {
        e.stopPropagation();
        const nodeId = node.id || node.getAttribute('data-id');
        const nodeText = node.querySelector('text')?.textContent || 'Unknown';
        
        this.pushEvent('node_clicked', {
          nodeId: nodeId,
          nodeText: nodeText,
          nodeType: this.getNodeType(node)
        });
        
        this.highlightNode(nodeId);
      });

      // Add hover effects
      node.addEventListener('mouseenter', () => {
        node.style.filter = 'brightness(1.1)';
      });

      node.addEventListener('mouseleave', () => {
        node.style.filter = '';
      });
    });
  },

  applyTransform(svg) {
    const { scale, translateX, translateY } = this.transform;
    svg.style.transform = `translate(${translateX}px, ${translateY}px) scale(${scale})`;
  },

  zoomIn() {
    this.transform.scale *= 1.2;
    this.transform.scale = Math.min(3, this.transform.scale);
    const svg = this.el.querySelector('svg');
    if (svg) this.applyTransform(svg);
  },

  zoomOut() {
    this.transform.scale *= 0.8;
    this.transform.scale = Math.max(0.1, this.transform.scale);
    const svg = this.el.querySelector('svg');
    if (svg) this.applyTransform(svg);
  },

  resetZoom() {
    this.transform = { scale: 1, translateX: 0, translateY: 0 };
    const svg = this.el.querySelector('svg');
    if (svg) this.applyTransform(svg);
  },

  exportSVG() {
    const svg = this.el.querySelector('svg');
    if (!svg) return;

    const svgData = new XMLSerializer().serializeToString(svg);
    const svgBlob = new Blob([svgData], { type: 'image/svg+xml;charset=utf-8' });
    const svgUrl = URL.createObjectURL(svgBlob);
    
    const downloadLink = document.createElement('a');
    downloadLink.href = svgUrl;
    downloadLink.download = `diagram-${Date.now()}.svg`;
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
    URL.revokeObjectURL(svgUrl);
  },

  toggleView() {
    const diagramWrapper = this.el.querySelector('.mermaid-diagram-wrapper');
    const codeWrapper = this.el.querySelector('.mermaid-code-wrapper');
    const chartIcon = this.el.querySelector('.chart-icon');
    const codeIcon = this.el.querySelector('.code-icon');

    if (diagramWrapper.classList.contains('hidden')) {
      // Show diagram, hide code
      diagramWrapper.classList.remove('hidden');
      codeWrapper.classList.add('hidden');
      chartIcon.classList.remove('hidden');
      codeIcon.classList.add('hidden');
    } else {
      // Show code, hide diagram
      diagramWrapper.classList.add('hidden');
      codeWrapper.classList.remove('hidden');
      chartIcon.classList.add('hidden');
      codeIcon.classList.remove('hidden');
    }
  },

  toggleTheme() {
    const currentTheme = mermaid.getConfig().theme;
    const newTheme = currentTheme === 'dark' ? 'default' : 'dark';

    mermaid.initialize({
      ...mermaid.getConfig(),
      theme: newTheme
    });

    this.renderDiagram();
  },

  highlightNode(nodeId) {
    // Remove previous highlights
    this.el.querySelectorAll('.node').forEach(node => {
      node.style.filter = '';
      node.style.outline = '';
    });

    // Highlight selected node
    const targetNode = this.el.querySelector(`#${nodeId}`) || 
                      this.el.querySelector(`[data-id="${nodeId}"]`);
    
    if (targetNode) {
      targetNode.style.outline = '3px solid #3B82F6';
      targetNode.style.filter = 'brightness(1.2)';
    }
  },

  getNodeType(node) {
    const classes = node.getAttribute('class') || '';
    if (classes.includes('start')) return 'start';
    if (classes.includes('end')) return 'end';
    if (classes.includes('decision')) return 'decision';
    if (classes.includes('process')) return 'process';
    return 'default';
  },

  updateDiagramStats(diagramElement) {
    const nodeCount = diagramElement.querySelectorAll('.node').length;
    const edgeCount = diagramElement.querySelectorAll('.edge').length;
    
    const statsElement = this.el.querySelector('.diagram-stats');
    if (statsElement) {
      statsElement.textContent = `${nodeCount} nodes, ${edgeCount} edges`;
    }
  },

  showError(container, message) {
    container.innerHTML = `
      <div class="bg-red-50 border border-red-200 rounded-lg p-4">
        <div class="flex">
          <svg class="w-5 h-5 text-red-400 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>
          </svg>
          <div>
            <h3 class="text-sm font-medium text-red-800">Diagram Rendering Error</h3>
            <p class="text-sm text-red-700 mt-1">${message}</p>
          </div>
        </div>
      </div>
    `;
  },

  escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  },

  escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  },

  cleanup() {
    // Remove event listeners and clean up resources
    if (this.transform) {
      this.transform = null;
    }
  }
};

// Export for use in app.js
export default MermaidHook;
