# Syntic

Syntic is a pure Haskell drawing engine with a strict hexagonal architecture. The immediate goal is to make the core model and use cases clean enough that future GUI, file format, rendering, and collaboration adapters can be added without leaking concerns across layers.

## Architecture

Dependency rule:

```text
Adapters -> Application -> Domain
```

Layer responsibilities:

- `Syntic.Domain.*` owns entities, value objects, invariants, and deterministic document transformations.
- `Syntic.Application.*` owns use cases and ports. It orchestrates domain behavior but does not know how storage, rendering, or UI are implemented.
- `Syntic.Adapter.*` owns concrete interpreters for ports. The first adapter is an in-memory document store so the whole flow stays testable and pure.
- `app/Main.hs` is the composition root. It wires ports to adapters and runs a tiny scenario.

## Initial Scope

The first slice models a drawable document with:

- a validated canvas size
- a background color
- a stacking-ordered shape collection
- pure commands for adding, moving, restyling, and removing shapes

This is intentionally small. It gives us a stable core before we add concerns like selection, layers, tools, undo/redo, rasterization, serialization, or a GUI.

## Ground Rules

- Domain modules stay pure and dependency-light.
- Application modules depend on ports, never adapters.
- Adapters implement ports and may be replaced without touching the core.
- The composition root is the only place where concrete adapters are wired in.
- Invalid states should be blocked with smart constructors when practical.

## Next Steps

Reasonable next increments:

1. Add document history and undo/redo as a separate application concern.
2. Introduce tool semantics such as rectangle tool, ellipse tool, and freehand stroke tool.
3. Add export/import ports for a native file format.
4. Add a rendering port and a pure scene graph projection.
5. Add a frontend adapter once the core behavior is stable.

