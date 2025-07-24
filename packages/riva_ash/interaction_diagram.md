## Pattern Interaction Diagram

The architectural patterns in Riva Ash work together to create a cohesive application architecture. The diagram below shows how these patterns interact:

```
+-----------------------------+
|  Phoenix Framework with     |
|  Ash Resource Pattern       |
|  (Foundation)               |
+------------+----------------+
             |
    +--------v--------+    +---------------------+
    | Policy-Based    |    | Shared Validation   |
    | Authorization   |    | Pattern             |
    | Pattern         |    |                     |
    +--------+--------+    +----------+----------+
             |                        |
    +--------v------------------------v------+
    |  Reactor Pattern for Complex Workflows  |
    +--------+------------------------+------+
             |                        |
    +--------v--------+    +----------v----------+
    | Atomic Design   |    |                     |
    | Component       |    |                     |
    | Architecture    |    |                     |
    +-----------------+    +---------------------+

Legend:
-> Direct interaction
--> Indirect interaction / Dependency
```

### Interaction Details

1. **Phoenix Framework with Ash Resource Pattern** serves as the foundation for all other patterns:
   - Provides the data layer and business logic core
   - All other patterns either directly or indirectly interact with Ash Resources

2. **Policy-Based Authorization Pattern** directly integrates with Ash Resources:
   - Defines access control rules for resource operations
   - Works with the Phoenix controllers to enforce authorization

3. **Shared Validation Pattern** is used within Ash Resources:
   - Ensures data integrity at the resource level
   - Provides reusable validation logic across different resources

4. **Reactor Pattern for Complex Workflows** orchestrates multiple Ash Resources:
   - Coordinates complex business processes that span multiple resources
   - Implements transactional consistency across operations
   - May use validation functions from the Shared Validation Pattern

5. **Atomic Design Component Architecture** consumes data from Ash Resources:
   - UI components display and interact with resource data
   - Forms submit data back to Ash Resources through Phoenix controllers
   - Components may trigger Reactor workflows for complex operations