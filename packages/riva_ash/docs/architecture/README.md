# Riva Ash Technical Documentation

This directory contains technical documentation specific to the Riva Ash package implementation, including architecture details, workflows, and visual diagrams.

## 📁 Documentation Structure

### Core Technical Documentation
- **[Ash Architecture TODOs](ash_architecture_todo.md)** - Tasks and rationale for aligning with Ash Framework best practices
- **[Chat TODOs](chat_todo.md)** - Chat-related development tasks and notes
- **[Glossary](glossary.md)** - Technical terms and definitions used in the Riva Ash system
- **[Property-Based Browser Testing](property_based_browser_testing.md)** - Testing approach using property-based browser testing
- **[Testing Guide](testing_guide.md)** - Comprehensive testing guidelines and best practices

### Visual Documentation (Mermaid Diagrams)
- **[Basic Reactor Flow](basic_reactor_flow.mmd)** - Flow diagram for basic reactor workflows
- **[Recurring Reservation Reactor Flow](recurring_reservation_reactor_flow.mmd)** - Complex reactor flow for recurring reservations
- **[Recurring Reservation Workflow](recurring_reservation_workflow.mmd)** - End-to-end workflow for recurring reservation processing

## 🔗 Connection to Main Documentation

This technical documentation complements the main project documentation located in the `docs/` directory. For comprehensive information about:

- **Project Overview & Setup**: See [`docs/GETTING_STARTED.md`](../docs/GETTING_STARTED.md)
- **Architecture Overview**: See [`docs/ARCHITECTURE.md`](../docs/ARCHITECTURE.md)
- **API Documentation**: See [`docs/API_REFERENCE.md`](../docs/API_REFERENCE.md)
- **Component Specifications**: See [`docs/components/README.md`](../docs/components/README.md)
- **Development Guidelines**: See [`docs/DEVELOPMENT_GUIDE.md`](../docs/DEVELOPMENT_GUIDE.md)

## 🎯 Quick Navigation

### For Developers
1. Start with [`ash_architecture_todo.md`](ash_architecture_todo.md) to understand current technical debt and improvement opportunities
2. Review [`testing_guide.md`](testing_guide.md) for testing standards and practices
3. Examine the Mermaid diagrams to understand complex workflows

### For Architects
1. Study the architecture TODOs to understand alignment with Ash Framework best practices
2. Review the workflow diagrams for system design insights
3. Consult the glossary for technical terminology

### For QA/Testers
1. Follow [`property_based_browser_testing.md`](property_based_browser_testing.md) for testing approach
2. Use [`testing_guide.md`](testing_guide.md) for comprehensive testing guidelines

## 📖 Visual Documentation

The Mermaid diagrams in this directory provide visual representations of complex workflows:

- **Basic Reactor Flow**: Shows the fundamental reactor pattern used throughout the system
- **Recurring Reservation Reactor Flow**: Illustrates the complex multi-step process for handling recurring reservations
- **Recurring Reservation Workflow**: Provides an end-to-end view of the recurring reservation creation and processing pipeline

These diagrams can be rendered in any Markdown viewer that supports Mermaid syntax.

## 🔍 Related Documentation

### Main Project Documentation
- [Complete Documentation Index](../docs/DOCUMENTATION_INDEX.md) - Overview of all documentation files
- [User Guide](../docs/user_guides/user_guide.md) - End-user documentation
- [Business Logic](../docs/business/user_actions_business_logic.md) - Business rules and data flow

### Component Documentation
- [UI Component Specifications](../docs/components/README.md) - Detailed component documentation
- [UI Guidelines](../docs/ui-guidelines.md) - Design system and styling guidelines

## 📝 Maintenance Guidelines

When adding new technical documentation:
1. Place files in the appropriate category above
2. Update this README with new entries
3. Add cross-references to related documentation
4. Follow established naming conventions
5. Ensure visual diagrams are properly integrated with explanatory text

## 🤝 Contributing

To contribute to this documentation:
1. Ensure all technical details are accurate and up-to-date
2. Include clear examples and code snippets where appropriate
3. Maintain consistency with the main project documentation
4. Test Mermaid diagrams to ensure they render correctly

---

*This README serves as the navigation hub for the Riva Ash package technical documentation. For questions about the overall project structure, refer to the main [README.md](../README.md) file.*