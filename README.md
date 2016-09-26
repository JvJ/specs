# specs

Specs is an experimental clojure game scripting framewok.  Specs stands for "Sequential Processes and Entity Component System".  It combines the communicating sequential processes (CSP) of clojure's core.async library with the entity-component-system (ECS) design pattern.

See docs/design.org for more detailed information.

## Overview and Examples

### ECS

Specs lets you define data as components:

```
(defcomponent Position [x y]) ;; A position component
(defcomponent Velocity [dx dy]) ;; A velocity component
```

And it lets you make entities that contain components:

```
(entity-with-id "dat boi"
     (->Position 100 100)
     (->Velocity 0 0))
```

Not only that, it lets you define functions that execute based on the components:

```
(defsys update-pos
  [ent]
  [p Position 
   {:keys [dx dy]} Velocity]
  (-> ent
      (update-in [Position :x] + dx)
      (update-in [Position :y] + dy)))
```

In the above example, we define a system function called update-pos.  The parameter list ```[ent]``` states that this function takes a single entity parameter.  The binding vector after that identifies the types of components that are required.  The binding form works just like clojure.core/let, with all supported destructuring.  In this case, we're binding ```p``` to the component position, and we're destructuring the velocity component into ```dx``` and ```dy```.  This system will only execute on entities that have both a Position and a Velocity component.

### Asynchronous processes

The other half of Specs is the asynchronous processes.

```
(defn set-vel
   [eid dx dy]
   (sysfn
     [ent]
     (match-eid eid)
     (assoc ent Velocity
            (assoc v :dx dx :dy dy))))
            
(defcontroller
   back-and-forth
   []
   (go-loop []
      (>! *ctrl* (set-velocity "dat boi" 10 0)
      (<! (timeout 1000))
      (>! *ctrl* (set-velocity "dat boi" -10 0)
      (<! (timeout 1000))
      (recur)))

```

In the above core, we define set-vel, a function that returns a function.  Given an entity id eid, and a dx and a dy, it returns a system function, just like update-pos that we had before.  The difference is that, instead of selecting entities based on their components, this system function uses match-eid to select entities based on their ID.  Since ID's are unique, this will be only one entity.

System functions, in addition to being used for updates, can also be used as messages.  The controller back-and-forth is a simple asychronous loop.  It sends a set-velocity message to the entity with id "dat boi", waits 1000 milliseconds, sets the velocity to the opposite direction, and waits another 1000 milliseconds.  This means that dat boi will move back and forth indefinitely, travelling for 1 second in each direction.

The end result is that the behaviour of the behaviour of the entity can be manipulated from asynchronous processes independent of the update cycle.  It's easy to see how more complex behaviours can be scripted, which, although they operate asynchronously, can be written just like linear code.

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
