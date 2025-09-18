---
applyTo: "*.pas"
---

# Pascal Coding Standards

## General Principles
* Use English for all code comments and documentation
* Prioritize code quality and maintainability
* Follow consistent formatting and naming conventions

## File Organization
* Unit names should follow PascalCase
* Uses clauses should be sorted alphabetically, each unit on a separate line
* Include a blank line between methods of a class for readability in implementation section, but not in interface section 

## Code Formatting
* Use 1 tab for indentation (no spaces)
* Add spaces around operators (e.g., `a := b + c`)
* Long lines (more than 120 characters) should be broken by a new line and an empty comment: `{ }`
* Functions without parameters should use empty parentheses: `()`
* Put `begin` and `end` blocks after keywords like `then` or `do`, even for single-line blocks

## Naming Conventions
* **Unit names**: Follow PascalCase
* **Class names**: Begin with 'T' and follow PascalCase (e.g., `TClassName`)
* **Field names**: Start with 'F' (e.g., `FFieldName`)
* **Constants**: Use UPPER_CASE with underscores (e.g., `CONSTANT_NAME`)
* **Public methods**: Follow PascalCase (e.g., `PublicMethod`)
* **Private methods**: Start with lowercase letter (e.g., `privateMethod`)
* **Local variables**: Start with lowercase letter (e.g., `localVar`)
* **Parameters**: Start with underscore and lowercase letter (e.g., `_parameterName`)

## Control Structures
* Loop variables should be declared as inline variables:
  ```pascal
  for var i: Integer := 0 to count do begin
      // loop body
  end;
  ```

## Collections
* prefer using Spring.Collections against System.Generics.Collections,
* Use `IList<T>` or `IDictionary<K, V>` for collections
* Use `TArrayEx<T>` where applicable for array operations

## Shared references
* Use `IShared<T>` from Spring4D for shared references to objects, to get rid of finally free blocks and avoid memory leaks
* Use `var obj := Shared.Make<T>(T.Create(...))` to create shared references

## Records
* Use `record` for simple data structures
* Use `New` for creating instances of records
* Use `T` prefix for record types (e.g., `TMyRecord`)

## Comments and Documentation
* Use english for comment texts
* Use single-line comments `//` for short explanations
* Use multi-line comments `{ }` for detailed descriptions
* Document complex algorithms and business logic

## Quality Assurance
* Build all relevant projects after changes to ensure compilation
* Ensure code follows these standards before submission
