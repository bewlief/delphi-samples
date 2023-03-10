Box2D.TestBed[]()
# Box2D.TestBed 


A sample that shows the use of the [Box2D](http://docwiki.embarcadero.com/RADStudio/en/Box2D) third party add-on.
## Contents



* [1 Location](#Location)
* [2 Description](#Description)
* [3 How to Use the Sample](#How_to_Use_the_Sample)
* [4 Implementation](#Implementation)

* [4.1 Main Form](#Main_Form)
* [4.2 Test](#Test)
* [4.3 Debug Draw](#Debug_Draw)

* [5 Uses](#Uses)
* [6 See Also](#See_Also)


## Location 

You can find the **TestBed** sample project at:
* **Start | Programs | Embarcadero RAD Studio Rio | Samples** and navigate to:

* `Object Pascal\Multi-Device Samples\Physics\TestBed`
* `CPP\Multi-Device Samples\Physics\TestBed`

* **GitHub Repository:**

* [https://github.com/Embarcadero/RADStudio11Demos/tree/main/Object%20Pascal/Multi-Device%20Samples/Physics/TestBed](https://github.com/Embarcadero/RADStudio11Demos/tree/main/Object%20Pascal/Multi-Device%20Samples/Physics/TestBed)
* [https://github.com/Embarcadero/RADStudio11Demos/tree/main/CPP/Multi-Device%20Samples/Physics/TestBed](https://github.com/Embarcadero/RADStudio11Demos/tree/main/CPP/Multi-Device%20Samples/Physics/TestBed)

## Description 

A sample that shows the use of the [Box2D](http://docwiki.embarcadero.com/RADStudio/en/Box2D) third party add-on.The application uses the following controls:

* `Timer1`: A control that implements the [TTimer](http://docwiki.embarcadero.com/Libraries/en/FMX.Types.TTimer) component.
* `MainPanel`: The main [panel](http://docwiki.embarcadero.com/Libraries/en/FMX.StdCtrls.TPanel) of the application.

* `PaintBox`: A [TPaintBox](http://docwiki.embarcadero.com/Libraries/en/FMX.Objects.TPaintBox) control where the application draws the graphics.
* `RightPanel`: The right [panel](http://docwiki.embarcadero.com/Libraries/en/FMX.StdCtrls.TPanel) of the application.

* `RightLayout`: A [TLayout](http://docwiki.embarcadero.com/Libraries/en/FMX.Layouts.TLayout) control that groups the controls on the right side of the application.

* `ButtonsLayout`: A [TLayout](http://docwiki.embarcadero.com/Libraries/en/FMX.Layouts.TLayout) control that groups the buttons.

* `PauseBtn`: Fires the `PauseBtnClick` event handler.
* `QuiBtn`: Fires the `QuiBtnClick` event handler.
* `RestartBtn`: Fires the `RestartBtnClick` event handler.
* `SingleStepBtn`: Fires the `SingleStepBtnClick` event handler.

* `DrawLayout`: A [TLayout](http://docwiki.embarcadero.com/Libraries/en/FMX.Layouts.TLayout) control that groups the check boxes that control the additional functionality of the application.

* `AABsChk`: Specifies whether the application displays the bounding boxes of the objects.
* `CenterOfMassesChk`: Specifies whether the application displays the center of masses of the objects.
* `ContactImpulsesChk`: Specifies whether the application displays the contact impulses. Only works if `ContactPointsChk` is enabled and `ContactNormalsChk` is disabled.
* `ContactNormalsChk`: Specifies whether the application displays the contact normals. Only works if `ContactPointsChk` is enabled and `ContactImpulsesChk` is disabled.
* `ContactPointsChk`: Specifies whether the application displays the contact points.
* `FrictionImpulsesChk`: Specifies whether the application displays the friction impulses. Only works if `ContactPointsChk` is enabled.
* `JointsChk`: Specifies whether the application displays the joints.
* `ProfilesChk`: Specifies whether the application displays profiling information. The application displays 3 values for each information: the current value, the average value, and the maximum value. Some of the tests provide additional profiling and/or statistical information. These are the values the profiling information includes:

*  step: The step duration (how long one iteration step needs to execute).
*  collide: The number of collisions.
*  solve: The duration of the solver (how long does it take for the solver to complete).
*  solve init: The duration of the integration solver (how long does it take for the integration solver to complete).
*  solve velocity: The duration of the velocity solver (how long does it take for the velocity solver to complete).
*  solve position: The duration of the position solver (how long does it take for the position solver to complete).
*  solveTOI: The duration of the collision solver (how long does it take for the collision solver to complete).
*  broad-phase: The duration of the broad-phase of the collision processing phase.

* `ShapesChk`: Specifies whether the application displays the shapes.
* `StatisticsChk`: Specifies whether the application displays statistical information about the current state, such as:

*  The number of bodies, contacts, and joints.
*  The number of proxies, the height, the balance and the quality.

* `EnableLayout`: A [TLayout](http://docwiki.embarcadero.com/Libraries/en/FMX.Layouts.TLayout) component that groups the check boxes that control nothing.

* `Sleep`: Specifies whether the bodies are allowed to go sleep. This is a global that specific tests can override. The application changes the color of shapes that go to sleep to grey.
* `SubStepping`: Specifies whether sub-stepping is enabled.
* `TimeOfImpact`: Specifies whether continuous collision detection (CCD) is enabled.
* `WarmStarting`: Specifies whether the application can cache some of the calculations in order to perform faster.

* `Line1`: A [TLine](http://docwiki.embarcadero.com/Libraries/en/FMX.Objects.TLine) control for visual separation.
* `TestLabel`: The [label](http://docwiki.embarcadero.com/Libraries/en/FMX.StdCtrls.TLabel) for the list of tests combo box.
* `TestList`: A [TComboBox](http://docwiki.embarcadero.com/Libraries/en/FMX.ListBox.TComboBox) control that holds the list of available tests.

* `RightSplitter`: A [TSplitter](http://docwiki.embarcadero.com/Libraries/en/FMX.StdCtrls.TSplitter) control for visual separation.

## How to Use the Sample 


1.  Navigate to one of the locations given above and open:

*  Delphi: **TestBed.dproj**.
*  C++: **TestBed.cbproj**.

2.  Press **F9** or choose **Run > Run**.

*  You can choose a different test from the list of tests.
*  Mark any of the check boxes to enable/disable that functionality.
*  Use the buttons to control the flow of the application.
You can perform the following global actions that apply to all tests:
*  Move objects: You can move any dynamic object by clicking on it with the mouse and then moving the mouse.
*  Bombs: There are two ways to launch a "bomb":

*  Press the `SPACE` key.
*  Hold shift, press the left mouse button and move the mouse. The distance of the mouse move corresponds to the velocity of the bomb. Release to launch the bomb.

*  Camera control: Use the following keys to control he movement of the camera:

*  (`SHIFT +) ARROW` key: Moves the camera.
*  Home: Resets the position of the camera.
Some tests provide additional actions and control over some options.The available tests are listed in the following table:


| Test Name            | Delphi                                                         | **C++**                                                        | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| -------------------- | -------------------------------------------------------------- | -------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| AddPair              | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Add Pair stress test.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| ApplyForce           | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows a number of objects on a surface that simulates ice.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| BasicSliderCrank     | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Simulates a slider crank. Use the following keys to control some options:* `F`}: Toggle friction. If you turn the motor off, you can see the effect of the friction option.* `M`: Toggle motor on/off.                                                                                                                                                                                                                                                                                                                                                           |
| BodyTypes            | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows the behaviour of bodies with different body types. Use the keys to toggle between the body types: * `D`: dynamic* `S`: static* `K`: kinematic                                                                                                                                                                                                                                                                                                                                                                                                              |
| Breakable            | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows two shapes that are joined together but can be broken into separate shapes.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Bridge               | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Simulates a bridge that consists of many shapes that are joined with a "revolute" joint.                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| BulletTest           | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows a collision between two shapes.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| Cantilever           | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates the use of the "weld" joint.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Car                  |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows the use of a "wheel" joint. Use the keys to control the car: * `A`: Turns the wheel to the left.* `S`: Stops the wheel.* `D`: Turns the wheel to the right.* `E`: Increases the refresh interval (speeds up the execution).* `Q`: Decreases the refresh interval (slows down the execution).                                                                                                                                                                                                                                                               |
| Chain                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Simulates a chain-like body.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| CharacterCollision   |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows various character collision scenarios. This test is useful for testing smooth collision on edge chains.                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| CollisionFiltering   |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates the use of collision filtering. These are the filters that this test uses:*  The 3 small shapes always collide.*  The 3 large shapes never collide.*  The boxes do not collide with triangles (except if both are small).                                                                                                                                                                                                                                                                                                                           |
|                      |                                                                |                                                                |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| CollisionProcessing  |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates the use of collision processing and body destruction. These are the rules that this test uses:*  A bigger body destroys a smaller body.*  A circle destroys a triangle and a polygon.*  A triangle destroys a polygon.                                                                                                                                                                                                                                                                                                                              |
| CompoundShapes       |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates several compound shapes.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| Confined             |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates the behaviour of objects in a confined space. Use the `C` key to create a new circle. You can see that the circles rearrange themselves to fit as well as possible inside the confined space.                                                                                                                                                                                                                                                                                                                                                       |
| Continuous Test      |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Simulates a drop test of a stick-shaped object and random speed and rotation angle.                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| Convex Hull          |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Test simulates a random convex hull generator. Press g to generate a new convex hull.                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| ConveyorBelt         |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | A simulation of a conveyor belt.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| Distance Test        |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Calculates and displays the shortest distance between two objects. You can control the position and the orientation of the small rectangle:* `A`: Moves the object to the left.* `D`: Moves the object to the right.* `S`: Moves the object down.* `W`: Moves the object up.* `Q`: Rotates the object counter-clockwise.* `E`: Rotates the object clockwise.                                                                                                                                                                                                     |
| Dominos              |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates a chain reaction that shows how the objects interact with each other.                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| DumpShell            |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Holds worlds dumped using b2World::Dump.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Dynamic Tree         |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Simulates a dynamic tree of bodies. You can control the some functionality of this test:* `A`: Toggles the automatic random movement of the bodies.* `C`: Creates a body.* `D`: Destroys a body.* `M`: Moves a random body.                                                                                                                                                                                                                                                                                                                                      |
| Edge Shapes          |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates a ray-cast that traverses a surface. To drop various objects to that surface, use the keys `1-5`.                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| Edge Test            |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Allows you to test the friction and behaviour of a rectangle and a circle on an uneven surface.                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Gears                |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Simulates various types of gears.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Heavy On Light       |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows a large and heavy object on that is placed on a small and light object.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Heavy On Light Two   |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows a small and heavy object on that is placed on a small and light object.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|                      |                                                                |                                                                |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Mobile               |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Simulates an unbalanced [mobile object](http://en.wikipedia.org/wiki/Mobile_(sculpture)).                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| MobileBalanced       |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Simulates a balanced [mobile object](http://en.wikipedia.org/wiki/Mobile_(sculpture)).                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| MotorJoint           |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows how to use a motor joint. You can use a motor joint to animate a dynamic body.                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| One-Sided Platform   |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates a one-sided platform. The objects can pass through the platform from below, but not from above.                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| Pinball              |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates bullet collision and provides an example of a gameplay scenario. It also uses a loop shape.                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| PolyCollision        |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Calculates and displays the amount of collision points between two objects. You can control the position and the orientation of the big rectangle:* `A`: Moves the object to the left.* `D`: Moves the object to the right.* `S`: Moves the object down.* `W`: Moves the object up.* `Q`: Rotates the object counter-clockwise.* `E`: Rotates the object clockwise.                                                                                                                                                                                              |
| Polygon Shapes       |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates stacking. It also shows how to use b2World::Query and b2TestOverlap. The application tests overlap and highlights up to 4 shapes that are inside the blue circle.                                                                                                                                                                                                                                                                                                                                                                                   |
| Prismatic            |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows a motor joint and allows you to control some aspects of the execution of the test. The following keys map to certain actions:* `L`: Toggles the limits of the movement of the object.* `M`: Toggles the motor on/off.* `S`: Specifies the direction of the motor. This setting has no effect if the motor is turned off.                                                                                                                                                                                                                                   |
| Pulleys              |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Simulates a pulley with two objects, one of which is heavier than the other.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| Pyramid              |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | A pyramid of rectangles.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Ray-Cast             |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates the use of the ray-cast feature. Use the keys `1 - 6` to drop objects to the stage. The `M` key toggles the ray-cast mode between:*  Closest: Finds the closest hit.*  Any: Finds any hit. This mode is appropriate to use when you do not care about the actual fixture and hit point.*  Multiple: Finds multiple hits.**Note:** The fixtures are not necessarily reported in order, so we might not capture the closest fixture first.The objects that are mapped to the `1` key do not obstruct the ray. This demonstrates the use of filtering. |
|                      |                                                                |                                                                |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Revolute             |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates the use of a motor and shows collision behaviour. Use the following keys for additional actions:* `M`: Toggle the motor on/off.* `L`: Stop the revolving object.                                                                                                                                                                                                                                                                                                                                                                                    |
| RopeJoint            |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows how you can use a rope joint to stabilize a chain of bodies with a heavy payload. The rope joint simply prevents excessive stretching and has no other effect. If you disable the rope joint, you can see that the Box2D solver has trouble supporting heavy bodies with light bodies. This test also shows how you can use contact filtering. Filtering is configured so that the payload does not collide with the chain.                                                                                                                                |
| SensorTest           |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates the use of sensor shapes. A sensor is a fixture that detects collision but does not produce a response.                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| Shape Editing        |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows basic shape behaviour. Use the `C` key to create a circle that overlaps with the square. Use the `D` key to destroy that circle.                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Slider Crank         |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows a slider crank. This test is useful for understanding constraints.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Sphere Stack         |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | A stack of spheres.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| Theo Jansen's Walker |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | A simulation of a Jansen Walker robot. You can control the movement of the robot:* `A`: Go left.* `S`: Stop.* `D`: Go right.* `M`: Toggle motor on/off.                                                                                                                                                                                                                                                                                                                                                                                                          |
| Tiles                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Stress tests the dynamic tree broad-phase. It also shows that tile based collision is not smooth due to Box2D not knowing about adjacency.                                                                                                                                                                                                                                                                                                                                                                                                                       |
| TimeOfImpact         |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows time of impact for bodies.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| Tumbler              |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | A simulation of a tumbler.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| Varying Friction     |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates the behaviour of objects with different friction levels.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| Varying Restitution  | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Shows the use of different restitution values. The restitution values determine how much an object bounces.                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Vertical Stack       |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Simulates a stack of objects and shows how bullet collision affects those objects. Press the `,` key to fire a bullet.                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Web                  |                                                                | ![YesC++11Feature.png](Readme%20Files/YesC%2B%2B11Feature.png) | Demonstrates the behaviour of distance joints in response to body destruction and joint destruction.                                                                                                                                                                                                                                                                                                                                                                                                                                                             |


## Implementation 


### Main Form 

The **MainForm** unit uses the **Test** and the **DebugDraw** units.The **MainForm** unit implements the following event handlers:

* `FormShow`: Calls `ResetView`, `DrawLayoutChanged`, and `TestChanged`.
* `FormCreate`: Creates an instance of the test settings and calls `LoadTests`.
* `FormDestroy`: Disables the timer control and frees the `TTest` class instance.
* `FormKeyDown`: Processes the action according to the key that you press. This is the list of global actions that you can trigger:

| Key               | Action                               |
| ----------------- | ------------------------------------ |
| `(SHIFT +) LEFT`  | Moves the test stage to the left.    |
| `(SHIFT +) RIGHT` | Moves the test stage to the right.   |
| `(SHIFT +) UP`    | Moves the test stage up.             |
| `(SHIFT +) DOWN`  | Moves the test stage down.           |
| `HOME`            | Resets the test stage position.      |
| `SPACE`           | Fires a "bomb" into the test object. |
| `P`               | Pauses the test.                     |
| `R`               | Resets and restarts the test.        |
| `ESC`             | Exits the application.               |


* `FormKeyUp`: Calls the `KeyboardUp` method of the `TTest` class.
* `FormResize`: Calls `ResetView`.
* `Timer1Timer`: Periodically refreshes the application.
* `PaintBoxPaint`: Fills the background of the [canvas](http://docwiki.embarcadero.com/Libraries/en/FMX.Graphics.TCanvas) and displays the title of the test. If the options are enabled, it also displays the statistics and the profiling data.
* `TestListChange`: Occurs when you select a test from the list of tests. Calls `TestChanged`.
* `PaintBoxMouseDown`: Occurs when you press a mouse button in the `PaintBox` area. If you press Shift + left mouse button, this method calls the `ShiftMouseDown` method of the `TTest` class; otherwise, it calls the `MouseDown` method of the `TTest` class.
* `PaintBoxMouseUp`: Occurs when you release the mouse button in the calls the `PaintBox` area. This method calls the `MouseUp` method of the `TTest` class.
* `PaintBoxMouseMove`: Occurs when you release the mouse button in the calls the `PaintBox` area. This method calls the `MouseUp` method of the `TTest` class.
* `DrawLayoutChanged`: Occurs when you move the mouse inside the `PaintBox` area. This method calls the `MouseMove` method of the `TTest` class.
* `PauseBtnClick`: Pauses the test.
* `QuiBtnClick`: Exits the application.
* `RestartBtnClick`: Resets and restarts the test.
* `SingleStepBtnClick`: Advances the test execution by one step. Only works when the test is paused.
The **MainForm** unit also implements the following methods:
* `LoadTests`: Populates the list of tests with the available tests and selects the first test on the list.
* `TestChanged`: Frees the current test and creates a new test based on your selection.
* `ResetView`: Centers the camera view according to the form.

### Test 

The **Test** unit acts as a base class for all the tests. It defines the following records:
* `Settings`: The test settings. You can control some of the settings at run time using the check boxes.
* `TestEntry`: A record that the main unit uses to populate the list of tests.
* `ContactPoint`: Contains all information that the application uses to calculate contact points, contact normals and contact impulses between bodies.
The **Test** unit also defines the following classes:
* `DestructionListener`: The class that frees the references to joints and fixtures after they are destroyed.
* `TTest`: A class that handles contact information. All the tests inherit this class. This class implements the following methods:

* `Create`: Creates the world and assigns the necessary listeners (destruction listener, contact listener).
* `Create`: Creates the world and assigns the necessary listeners (destruction listener, contact listener).
* `Destroy`: Destroys the world and frees the listeners that the constructor assigns.
* `DrawTitle`: Draws the provided title text string.
* `Step`: Advances the test execution. Handles the objects according to the test settings (draws shapes, joints, centers of masses, bounding boxes, statistics information).
* `Keyboard`: A virtual method that the descendant classes use to handle keyboard input.
* `KeyboardUp`: A virtual method that the descendant classes use to handle keyboard input.
* `ShiftMouseDown`: Calls `SpawnBomb`.
* `MouseDown`: Creates a small box shape at the location of the mouse pointer, if that location is inside of any body on the stage. Creates a joint that connects the created shape to your mouse pointer.
* `MouseUp`: If a joint exists that connects the mouse pointer to a shape, this method destroys that joint. If a bomb is spawning (see `SpawnBomb`), this method calls `CompleteBombSpawn`.
* `MouseMove`: If a joint exists that connects the mouse pointer to a shape (see `MouseDown`), this method extends that joint. Because this is a joint that acts as a spring, you can pull object on the stage by clicking on them with the mouse and moving the mouse.
* `LaunchBomb`: Creates a circle shape that acts like a bullet and sets the velocity of that shape.
* `SpawnBomb`: Sets the spawn point of the bomb and raises the flag that marks that a bomb spawn is in progress.
* `CompleteBombSpawn`: Calculates the bomb speed and calls `LaunchBomb`.
* `JointDestroyed`: A virtual method that the descendant classes use to handle the destruction of joints.
* `BeginContact`: Occurs when two fixtures begin to touch.
* `EndContact`: Occurs when two fixtures stop touching.
* `PreSolve`: Occurs after a contact is updated. This allows you to inspect a contact before it goes to the solver.
* `PostSolve`: Lets you inspect a contact after the solver is finished. This is useful for inspecting impulses.
* `ShiftOrigin`: Changes the origin point for this world.

### Debug Draw 

A unit that implements the `TDebugDraw` class that provides methods for drawing and for debugging.
## Uses 


* [FMX.Graphics.TCanvas](http://docwiki.embarcadero.com/Libraries/en/FMX.Graphics.TCanvas)

* [System.Math.Vectors](http://docwiki.embarcadero.com/Libraries/en/System.Math.Vectors)

## See Also 


* [Box2D](http://docwiki.embarcadero.com/RADStudio/en/Box2D)
* [Using Box2D in Delphi Applications](http://docwiki.embarcadero.com/RADStudio/en/Using_Box2D_in_Delphi_Applications)
* [Using Box2D in C++Builder Applications](http://docwiki.embarcadero.com/RADStudio/en/Using_Box2D_in_C%2B%2BBuilder_Applications)

* [Box2D User Manual](http://box2d.org/manual.pdf)





