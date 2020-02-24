module Verishot.Test.SVG

open Expecto
open Verishot.Test.Util
open Verishot.SVG

let svg1 = Circle ((1., 2.), 3., ["class", "circ-class"], Some "svg1")
let svg2 = Rectangle ((1., 2.), (3.,4.), ["class", "rect-class"], Some "svg2")
let svg3 = Text ((1., 2.), "svg3text", ["class", "text-class"], Some "svg3")
let svg4 = Polyline ([(0.,0.); (1.,2.); (3.,4.)], ["class", "polyline-class"], Some "svg4")
let svg5 = Group ([svg1; svg2; svg3; svg4], ["class", "group-class"], Some "svg5")
let svg6 = Link ("/link", [svg1; svg2; svg3; svg4], ["class", "link-class"], Some "svg6")

let groupSVGTests = 
    [
        "empty",
            ([], None, []),
                Group ([], [], None)
        "test props and title",
            ([("foo1", "bar1"); ("foo2", "bar2")], Some "title", []),
                Group ([], [("foo1", "bar1"); ("foo2", "bar2")], Some "title")
        "simple 1",
            ([("foo1", "bar1"); ("foo2", "bar2")], Some "title", [svg1]),
                Group ([svg1], [("foo1", "bar1"); ("foo2", "bar2")], Some "title")
        "simple 4",
            ([("foo1", "bar1"); ("foo2", "bar2")], Some "title", [svg1; svg2; svg3; svg4]),
                Group ([svg1; svg2; svg3; svg4], [("foo1", "bar1"); ("foo2", "bar2")], Some "title")
        "simple 6",
            ([("foo1", "bar1"); ("foo2", "bar2")], Some "title", [svg1; svg2; svg3; svg4; svg5; svg6]),
                Group ([svg1; svg2; svg3; svg4; svg5; svg6], [("foo1", "bar1"); ("foo2", "bar2")], Some "title")
    ]

let linkSVGTests = 
    [
        "empty",
            ("", [], None, []),
                Link ("", [], [], None)
        "test href, props and title",
            ("/link", [("foo1", "bar1"); ("foo2", "bar2")], Some "title", []),
                Link ("/link", [], [("foo1", "bar1"); ("foo2", "bar2")], Some "title")
        "simple 1",
            ("/link", [("foo1", "bar1"); ("foo2", "bar2")], Some "title", [svg1]),
                Link ("/link", [svg1], [("foo1", "bar1"); ("foo2", "bar2")], Some "title")
        "simple 4",
            ("/link", [("foo1", "bar1"); ("foo2", "bar2")], Some "title", [svg1; svg2; svg3; svg4]),
                Link ("/link", [svg1; svg2; svg3; svg4], [("foo1", "bar1"); ("foo2", "bar2")], Some "title")
        "simple 6",
            ("/link", [("foo1", "bar1"); ("foo2", "bar2")], Some "title", [svg1; svg2; svg3; svg4; svg5; svg6]),
                Link ("/link", [svg1; svg2; svg3; svg4; svg5; svg6], [("foo1", "bar1"); ("foo2", "bar2")], Some "title")
    ]

(* NB This assumes unitPx = 12. *)
let TOPXTests = 
    [
        "0.",
            (0.),
                "0.0"
        "1.",
            (1.),
                "12.0"
        "69420.",
            (69420.),
                "833040.0"
        "0.5",
            (0.5),
                "6.0"
        "0.6969",
            (0.6969),
                "8.4"
        "-23.2323",
            (-23.2323),
                "-278.8"
    ]

let UPROPSTests =
    [
        "empty",
            [],
                ""
        "simple 1",
            [("foo", "bar")],
                "foo='bar'"
        "simple 3",
            [("foo1", "bar1"); ("foo2", "bar2"); ("foo3", "bar3")],
                "foo1='bar1' foo2='bar2' foo3='bar3'"
    ]

let UPOINTSTests =
    [
        "empty",
            [],
                ""
        "simple 1",
            [(0., 0.)],
                "0.0,0.0"
        "simple 3",
            [(1., 2.); (3., 4.); (5., 6.)],
                "12.0,24.0 36.0,48.0 60.0,72.0"
        "complicated 2",
            [(-1., 0.2222); (0.423, -69420.123)],
                "-12.0,2.7 5.1,-833041.5"
    ]

let PTITLETests =
    [
        "empty",
            None,
                ""
        "simple ",
            Some "foo",
                "<title>foo</title>"
    ]

let OUTPUTSVGLISTTests =
    [
        "empty",
            [],
                ""
        "simple 1",
            [svg1],
            "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>"
        "simple 2",
            [svg1; svg2],
                "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "<rect x='12.0' y='24.0' width='36.0' height='48.0' class='rect-class'><title>svg2</title></rect>"
    ]
    
let outputSVGTests =
    let svg1nc = Circle ((1., 2.), 3., [], Some "svg1")
    let svg2nc = Rectangle ((1., 2.), (3.,4.), [], Some "svg2")
    let svg3nc = Text ((1., 2.), "svg3text", [], Some "svg3")
    let svg4nc = Polyline ([(0.,0.); (1.,2.); (3.,4.)], [], Some "svg4")
    let svg5nc = Group ([svg1; svg2; svg3; svg4], [], Some "svg5")
    let svg6nc = Link ("/link", [svg1; svg2; svg3; svg4], [], Some "svg6")

    let svg1nt = Circle ((1., 2.), 3., ["class", "circ-class"], None)
    let svg2nt = Rectangle ((1., 2.), (3.,4.), ["class", "rect-class"], None)
    let svg3nt = Text ((1., 2.), "svg3text", ["class", "text-class"], None)
    let svg4nt = Polyline ([(0.,0.); (1.,2.); (3.,4.)], ["class", "polyline-class"], None)
    let svg5nt = Group ([svg1; svg2; svg3; svg4], ["class", "group-class"], None)
    let svg6nt = Link ("/link", [svg1; svg2; svg3; svg4], ["class", "link-class"], None)

    let emptyGroupChildren = Group ([], ["class", "empty-group-class"], None)
    let emptyLinkChildren = Link ("/link", [], ["class", "empty-link-class"], None)

    let emptyLink = Link ("", [svg1; svg2; svg3; svg4], ["class", "link-class"], None)
    let emptyText = Text ((1., 2.), "", ["class", "text-class"], Some "svg3")

    let svg1ManyProps = Circle ((1., 2.), 3., [("foo1", "bar1"); ("foo2", "bar2"); ("foo3", "bar3")], Some "svg1")
    [
        "circle",
            svg1,
                "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>"
        "circle with no props",
            svg1nc,
                "<circle cx='12.0' cy='24.0' r='36.0' ><title>svg1</title></circle>"
        "circle with no title",
            svg1nt,
                "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'></circle>"
        "rect",
            svg2,
                "<rect x='12.0' y='24.0' width='36.0' height='48.0' class='rect-class'><title>svg2</title></rect>"
        "rect with no props",
            svg2nc,
                "<rect x='12.0' y='24.0' width='36.0' height='48.0' ><title>svg2</title></rect>"
        "rect with no title",
            svg2nt,
                "<rect x='12.0' y='24.0' width='36.0' height='48.0' class='rect-class'></rect>"
        "text",
            svg3,
                "<text x='12.0' y='24.0' class='text-class'>svg3text<title>svg3</title></text>"
        "text with no props",
            svg3nc,
                "<text x='12.0' y='24.0' >svg3text<title>svg3</title></text>"
        "text with no title",
            svg3nt,
                "<text x='12.0' y='24.0' class='text-class'>svg3text</text>"
        "polyline",
            svg4,
                "<polyline points='0.0,0.0 12.0,24.0 36.0,48.0' class='polyline-class'><title>svg4</title></polyline>"
        "polyline with no props",
            svg4nc,
                "<polyline points='0.0,0.0 12.0,24.0 36.0,48.0' ><title>svg4</title></polyline>"
        "polyline with no title",
            svg4nt,
                "<polyline points='0.0,0.0 12.0,24.0 36.0,48.0' class='polyline-class'></polyline>"
        "group",
            svg5,
                "<g class='group-class'><title>svg5</title>\n"
                + "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "<rect x='12.0' y='24.0' width='36.0' height='48.0' class='rect-class'><title>svg2</title></rect>\n"
                + "<text x='12.0' y='24.0' class='text-class'>svg3text<title>svg3</title></text>\n"
                + "<polyline points='0.0,0.0 12.0,24.0 36.0,48.0' class='polyline-class'><title>svg4</title></polyline>\n"
                + "</g>"
        "group with no props",
            svg5nc,
                "<g ><title>svg5</title>\n"
                + "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "<rect x='12.0' y='24.0' width='36.0' height='48.0' class='rect-class'><title>svg2</title></rect>\n"
                + "<text x='12.0' y='24.0' class='text-class'>svg3text<title>svg3</title></text>\n"
                + "<polyline points='0.0,0.0 12.0,24.0 36.0,48.0' class='polyline-class'><title>svg4</title></polyline>\n"
                + "</g>"
        "group with no title",
            svg5nt,
                "<g class='group-class'>\n"
                + "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "<rect x='12.0' y='24.0' width='36.0' height='48.0' class='rect-class'><title>svg2</title></rect>\n"
                + "<text x='12.0' y='24.0' class='text-class'>svg3text<title>svg3</title></text>\n"
                + "<polyline points='0.0,0.0 12.0,24.0 36.0,48.0' class='polyline-class'><title>svg4</title></polyline>\n"
                + "</g>"
        "link",
            svg6,
                "<a href='/link' class='link-class'><title>svg6</title>\n"
                + "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "<rect x='12.0' y='24.0' width='36.0' height='48.0' class='rect-class'><title>svg2</title></rect>\n"
                + "<text x='12.0' y='24.0' class='text-class'>svg3text<title>svg3</title></text>\n"
                + "<polyline points='0.0,0.0 12.0,24.0 36.0,48.0' class='polyline-class'><title>svg4</title></polyline>\n"
                + "</a>"
        "link with no props",
            svg6nc,
                "<a href='/link' ><title>svg6</title>\n"
                + "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "<rect x='12.0' y='24.0' width='36.0' height='48.0' class='rect-class'><title>svg2</title></rect>\n"
                + "<text x='12.0' y='24.0' class='text-class'>svg3text<title>svg3</title></text>\n"
                + "<polyline points='0.0,0.0 12.0,24.0 36.0,48.0' class='polyline-class'><title>svg4</title></polyline>\n"
                + "</a>"
        "link with no title",
            svg6nt,
                "<a href='/link' class='link-class'>\n"
                + "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "<rect x='12.0' y='24.0' width='36.0' height='48.0' class='rect-class'><title>svg2</title></rect>\n"
                + "<text x='12.0' y='24.0' class='text-class'>svg3text<title>svg3</title></text>\n"
                + "<polyline points='0.0,0.0 12.0,24.0 36.0,48.0' class='polyline-class'><title>svg4</title></polyline>\n"
                + "</a>"
        "group with no children",
            emptyGroupChildren,
                "<g class='empty-group-class'>\n"
                + "\n"
                + "</g>"
        "group with no link",
            emptyLinkChildren,
                "<a href='/link' class='empty-link-class'>\n"
                + "\n"
                + "</a>"
        "empty link",
            emptyLink,
                "<a href='' class='link-class'>\n"
                + "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "<rect x='12.0' y='24.0' width='36.0' height='48.0' class='rect-class'><title>svg2</title></rect>\n"
                + "<text x='12.0' y='24.0' class='text-class'>svg3text<title>svg3</title></text>\n"
                + "<polyline points='0.0,0.0 12.0,24.0 36.0,48.0' class='polyline-class'><title>svg4</title></polyline>\n"
                + "</a>"
        "empty text",
            emptyText,
                "<text x='12.0' y='24.0' class='text-class'><title>svg3</title></text>"  
        "circle with many props",     
            svg1ManyProps,
                "<circle cx='12.0' cy='24.0' r='36.0' foo1='bar1' foo2='bar2' foo3='bar3'><title>svg1</title></circle>"
    ]

let getDimensionElemTests =
    [
        "circle",
            svg1,
                ((-2., -1.), (4., 5.))
        "rect",
            svg2,
                ((1., 2.), (4., 6.))
        "text",
            svg3,
                ((1., 2.), (1., 2.))
        "polyline",
            svg4,
                ((0., 0.), (3., 4.))
        "group",
            svg5,
                ((-2., -1.), (4., 6.))
        "link",
            svg6,   
                ((-2., -1.), (4., 6.))
    ]

let getDimensionElemListTests = 
    [
        "empty",
            [],
                ((0., 0.), (0., 0.))
        "svg1",
            [svg1],
                ((-2., -1.), (4., 5.))
        "list",
            [svg1; svg2; svg3; svg4],
                ((-2., -1.), (4., 6.))
    ]

let translateCoordTests = 
    let zeroOffset = 0., 0.
    let positiveOffset = 1., 2.
    let negativeOffset = -1., -2.
    [
        "basic 0",
            (zeroOffset, (0., 0.)),
                (0., 0.)
        "basic positive",
            (positiveOffset, (3., 4.)),
                (4., 6.)
        "basic negative",
            (negativeOffset, (-3., -4.)),
                (-4., -6.)
    ]

[<Tests>]
let translateCoordTestList =
    testList "translateCoord" <| 
        (translateCoordTests 
         |> List.map (processIntoAsyncTestList2 translateCoord))

let translateSVGListTests = 
    let svgList = [svg1; svg2; svg3; svg4]
    let zeroOffset = 0., 0.
    let positiveOffset = 1., 2.
    let negativeOffset = -10., -20.
    [
        "0 offset",
            (zeroOffset, svgList),
                svgList
        "positive offset",
            (positiveOffset, svgList),
                [
                    Circle ((2., 4.), 3., ["class", "circ-class"], Some "svg1")
                    Rectangle ((2., 4.), (3.,4.), ["class", "rect-class"], Some "svg2")
                    Text ((2., 4.), "svg3text", ["class", "text-class"], Some "svg3")
                    Polyline ([(1.,2.); (2.,4.); (4.,6.)], ["class", "polyline-class"], Some "svg4")
                ]
        "negative offset",
            (negativeOffset, svgList),
                [
                    Circle ((-9., -18.), 3., ["class", "circ-class"], Some "svg1")
                    Rectangle ((-9., -18.), (3., 4.), ["class", "rect-class"], Some "svg2")
                    Text ((-9., -18.), "svg3text", ["class", "text-class"], Some "svg3")
                    Polyline ([(-10., -20.); (-9., -18.); (-7., -16.)], ["class", "polyline-class"], Some "svg4")
                ]
    ]

[<Tests>]
let translateSVGListTestList =
    testList "translateSVGList" <| 
        (translateSVGListTests 
         |> List.map (processIntoAsyncTestList2 translateSVGList))

let translateSVGTests = 
    let svg1pos = Circle ((2., 4.), 3., ["class", "circ-class"], Some "svg1")
    let svg2pos = Rectangle ((2., 4.), (3.,4.), ["class", "rect-class"], Some "svg2")
    let svg3pos = Text ((2., 4.), "svg3text", ["class", "text-class"], Some "svg3")
    let svg4pos = Polyline ([(1.,2.); (2., 4.); (4., 6.)], ["class", "polyline-class"], Some "svg4")
    let svg5pos = Group ([svg1pos; svg2pos; svg3pos; svg4pos], ["class", "group-class"], Some "svg5")
    let svg6pos = Link ("/link", [svg1pos; svg2pos; svg3pos; svg4pos], ["class", "link-class"], Some "svg6")
    
    let svg1neg = Circle ((0., 0.), 3., ["class", "circ-class"], Some "svg1")
    let svg2neg = Rectangle ((0., 0.), (3.,4.), ["class", "rect-class"], Some "svg2")
    let svg3neg = Text ((0., 0.), "svg3text", ["class", "text-class"], Some "svg3")
    let svg4neg = Polyline ([(-1., -2.); (0.,0.); (2.,2.)], ["class", "polyline-class"], Some "svg4")
    let svg5neg = Group ([svg1neg; svg2neg; svg3neg; svg4neg], ["class", "group-class"], Some "svg5")
    let svg6neg = Link ("/link", [svg1neg; svg2neg; svg3neg; svg4neg], ["class", "link-class"], Some "svg6")
    
    let zeroOffset = 0., 0.
    let positiveOffset = 1., 2.
    let negativeOffset = -1., -2.

    [
        "0 offset circ",
            (zeroOffset, svg1),
                svg1
        "0 offset rect",
            (zeroOffset, svg2),
                svg2
        "0 offset text",
            (zeroOffset, svg3),
                svg3
        "0 offset polyline",
            (zeroOffset, svg4),
                svg4
        "0 offset group",
            (zeroOffset, svg5),
                svg5
        "0 offset link",
            (zeroOffset, svg6),
                svg6
        "pos offset circ",
            (positiveOffset, svg1),
                svg1pos
        "pos offset rect",
            (positiveOffset, svg2),
                svg2pos
        "pos offset text",
            (positiveOffset, svg3),
                svg3pos
        "pos offset polyline",
            (positiveOffset, svg4),
                svg4pos
        "pos offset group",
            (positiveOffset, svg5),
                svg5pos
        "pos offset link",
            (positiveOffset, svg6),
                svg6pos
        "neg offset circ",
            (negativeOffset, svg1),
                svg1neg
        "neg offset rect",
            (negativeOffset, svg2),
                svg2neg
        "neg offset text",
            (negativeOffset, svg3),
                svg3neg
        "neg offset polyline",
            (negativeOffset, svg4),
                svg4neg
        "neg offset group",
            (negativeOffset, svg5),
                svg5neg
        "neg offset link",
            (negativeOffset, svg6),
                svg6neg
          
    ]

[<Tests>]
let translateSVGTestList =
    testList "translateSVG" <| 
        (translateSVGTests 
         |> List.map (processIntoAsyncTestList2 translateSVG))
    
let getGridTests =
    [
        "basic1",
            ((0., 0.), (2., 2.)),
                Group ([Circle ((0.0, 0.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((0.0, 1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((0.0, 2.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((1.0, 0.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((1.0, 1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((1.0, 2.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((2.0, 0.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((2.0, 1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((2.0, 2.0),0.125,[("style", "fill: #E0E0E0;")], None)], [], None)
        "basic2",
            ((-1., -1.), (1., 1.)),
                Group ([Circle ((-1.0, -1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((-1.0, 0.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((-1.0, 1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((0.0, -1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((0.0, 0.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((0.0, 1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((1.0, -1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((1.0, 0.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((1.0, 1.0),0.125,[("style", "fill: #E0E0E0;")], None)], [], None)
        "basic3",
            ((-1., -0.), (2., 1.)),
                Group ([Circle ((-1.0, 0.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((-1.0, 1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((0.0, 0.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((0.0, 1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((1.0, 0.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((1.0, 1.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((2.0, 0.0),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((2.0, 1.0),0.125,[("style", "fill: #E0E0E0;")], None)], [], None)
        "basic4",
            ((-1.5, -1.5), (0.5, 0.5)),
                Group ([Circle ((-1.5, -1.5),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((-1.5, -0.5),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((-1.5, 0.5),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((-0.5, -1.5),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((-0.5, -0.5),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((-0.5, 0.5),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((0.5, -1.5),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((0.5, -0.5),0.125,[("style", "fill: #E0E0E0;")], None);
                Circle ((0.5, 0.5),0.125,[("style", "fill: #E0E0E0;")], None)], [], None)
    ]

let outputTests = 
    [
        "basic1, no style, no grid",
            (svg1, "", false),
                "<?xml version='1.0' encoding='UTF-8'?>\n"
                + "<!-- SVG Output - Verishot Simulator -->\n"
                + "<svg xmlns='http://www.w3.org/2000/svg' width='168.0' height='168.0' viewBox='-72.0 -60.0 144.0 156.0'>\n"
                + "<style type='text/css'>\n"
                + "\n"
                + "</style>\n"
                + "\n"
                + "<rect x='-48.0' y='-36.0' width='120.0' height='120.0' style='fill: none; stroke: black;'></rect>\n"
                + "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "</svg>"
        "basic2, style, no grid",
            (svg1, "FOOBAR", false),
                "<?xml version='1.0' encoding='UTF-8'?>\n"
                + "<!-- SVG Output - Verishot Simulator -->\n"
                + "<svg xmlns='http://www.w3.org/2000/svg' width='168.0' height='168.0' viewBox='-72.0 -60.0 144.0 156.0'>\n"
                + "<style type='text/css'>\n"
                + "FOOBAR\n"
                + "</style>\n"
                + "\n"
                + "<rect x='-48.0' y='-36.0' width='120.0' height='120.0' style='fill: none; stroke: black;'></rect>\n"
                + "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "</svg>"
        "basic3, style, grid",
            (svg1, "FOOBAR", true),
                "<?xml version='1.0' encoding='UTF-8'?>\n"
                + "<!-- SVG Output - Verishot Simulator -->\n"
                + "<svg xmlns='http://www.w3.org/2000/svg' width='168.0' height='168.0' viewBox='-72.0 -60.0 144.0 156.0'>\n"
                + "<style type='text/css'>\n"
                + "FOOBAR\n"
                + "</style>\n"
                + "<g >\n"
                + "<circle cx='-36.0' cy='-24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-36.0' cy='-12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-36.0' cy='0.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-36.0' cy='12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-36.0' cy='24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-36.0' cy='36.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-36.0' cy='48.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-36.0' cy='60.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-36.0' cy='72.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-24.0' cy='-24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-24.0' cy='-12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-24.0' cy='0.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-24.0' cy='12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-24.0' cy='24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-24.0' cy='36.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-24.0' cy='48.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-24.0' cy='60.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-24.0' cy='72.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-12.0' cy='-24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-12.0' cy='-12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-12.0' cy='0.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-12.0' cy='12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-12.0' cy='24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-12.0' cy='36.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-12.0' cy='48.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-12.0' cy='60.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='-12.0' cy='72.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='0.0' cy='-24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='0.0' cy='-12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='0.0' cy='0.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='0.0' cy='12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='0.0' cy='24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='0.0' cy='36.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='0.0' cy='48.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='0.0' cy='60.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='0.0' cy='72.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='12.0' cy='-24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='12.0' cy='-12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='12.0' cy='0.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='12.0' cy='12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='12.0' cy='24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='12.0' cy='36.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='12.0' cy='48.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='12.0' cy='60.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='12.0' cy='72.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='24.0' cy='-24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='24.0' cy='-12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='24.0' cy='0.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='24.0' cy='12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='24.0' cy='24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='24.0' cy='36.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='24.0' cy='48.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='24.0' cy='60.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='24.0' cy='72.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='36.0' cy='-24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='36.0' cy='-12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='36.0' cy='0.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='36.0' cy='12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='36.0' cy='24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='36.0' cy='36.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='36.0' cy='48.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='36.0' cy='60.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='36.0' cy='72.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='48.0' cy='-24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='48.0' cy='-12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='48.0' cy='0.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='48.0' cy='12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='48.0' cy='24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='48.0' cy='36.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='48.0' cy='48.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='48.0' cy='60.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='48.0' cy='72.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='60.0' cy='-24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='60.0' cy='-12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='60.0' cy='0.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='60.0' cy='12.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='60.0' cy='24.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='60.0' cy='36.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='60.0' cy='48.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='60.0' cy='60.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "<circle cx='60.0' cy='72.0' r='1.5' style='fill: #E0E0E0;'></circle>\n"
                + "</g>\n"
                + "<rect x='-48.0' y='-36.0' width='120.0' height='120.0' style='fill: none; stroke: black;'></rect>\n"
                + "<circle cx='12.0' cy='24.0' r='36.0' class='circ-class'><title>svg1</title></circle>\n"
                + "</svg>"
    ]

[<Tests>]
let groupSVGTestList =
    testList "groupSVG" <| 
        (groupSVGTests 
         |> List.map (processIntoAsyncTestList3 groupSVG))

[<Tests>]
let linkSVGTestList =
    testList "linkSVG" <| 
        (linkSVGTests 
         |> List.map (processIntoAsyncTestList4 linkSVG))

[<Tests>]
let TOPXTestList =
    testList "TOPX" <| 
        (TOPXTests 
         |> List.map (processIntoAsyncTestList1 (|TOPX|)))

[<Tests>]
let UPROPSTestList =
    testList "UPROPS" <| 
        (UPROPSTests 
         |> List.map (processIntoAsyncTestList1 (|UPROPS|)))

[<Tests>]
let UPOINTSTestList =
    testList "UPOINTS" <| 
        (UPOINTSTests 
         |> List.map (processIntoAsyncTestList1 (|UPOINTS|)))

[<Tests>]
let PTITLETestList =
    testList "PTITLE" <| 
        (PTITLETests 
         |> List.map (processIntoAsyncTestList1 (|PTITLE|)))

[<Tests>]
let OUTPUTSVGLISTTestList =
    testList "OUTPUTSVGLIST" <| 
        (OUTPUTSVGLISTTests 
         |> List.map (processIntoAsyncTestList1 (|OUTPUTSVGLIST|)))
 
[<Tests>]
let outputSVGTestList =
    testList "outputSVG" <| 
        (outputSVGTests 
         |> List.map (processIntoAsyncTestList1 outputSVG))

[<Tests>]
let getDimensionElemTestList =
    testList "getDimensionElem" <| 
        (getDimensionElemTests 
         |> List.map (processIntoAsyncTestList1 getDimensionElem))

[<Tests>]
let getDimensionElemListTestList =
    testList "getDimensionElemList" <| 
        (getDimensionElemListTests 
         |> List.map (processIntoAsyncTestList1 getDimensionElemList))

[<Tests>]
let getGridTestList =
    testList "getGrid" <| 
        (getGridTests 
         |> List.map (processIntoAsyncTestList1 getGrid))

[<Tests>]
let outputTestList =
    testList "output" <| 
        (outputTests 
         |> List.map (processIntoAsyncTestList3 output))