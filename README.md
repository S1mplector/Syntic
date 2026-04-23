# Syntic

Syntic is a pure Haskell watercolor engine. The immediate goal is to make the core model and use cases clean enough that future GUI, file format, rendering, and collaboration adapters can be added without leaking concerns across layers.

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

## Current Scope

The engine models a layered, watercolor-aware document with:

- a validated canvas size and background color
- **layers** with blend mode, opacity, and visibility — essential for watercolor techniques like wet-on-dry glazing and separate washes
- **watercolor brush model** (`Syntic.Domain.Brush`) with tip type, size, opacity, flow, wetness, pigment density, granulation, and edge softness
- **pressure-aware brush strokes** (`Syntic.Domain.BrushStroke`) composed of sample points carrying position and tablet pressure
- geometric shapes (rectangles, ellipses, polylines) alongside brush strokes in the same layer
- pure commands for adding/removing layers, adding/moving/restyling/removing shapes within a targeted layer
- **undo/redo history** (`Syntic.Application.History`) as a separate application concern with snapshot-based state tracking

## Ground Rules

- Domain modules stay pure and dependency-light.
- Application modules depend on ports, never adapters.
- Adapters implement ports and may be replaced without touching the core.
- The composition root is the only place where concrete adapters are wired in.
- Invalid states should be blocked with smart constructors when practical.

## Next Steps

Reasonable next increments:

1. Introduce tool semantics — rectangle tool, ellipse tool, brush tool — that map input events to document commands.
2. Add a watercolor simulation module: pigment diffusion, wet-on-wet bleeding, granulation texture, and drying.
3. Add export/import ports for a native file format.
4. Add a rendering port and a pure scene graph projection that respects layer blend modes and brush parameters.
5. Add a frontend adapter (GUI or terminal preview) once the core behavior is stable.

