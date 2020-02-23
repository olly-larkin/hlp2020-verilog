module Verishot.Test.SVG

open Expecto
open Verishot.Test.Util
open Verishot.SVG

let svg1 = Circle ((1., 2.), 3., ["class", "circ-class"], "svg1")
let svg2 = Rectangle ((1., 2.), (3.,4.), ["class", "rect-class"], "svg2")
let svg3 = Text ((1., 2.), "svg3text", ["class", "text-class"], "svg3")
let svg4 = Polyline ([(0.,0.); (1.,2.); (3.,4.)], ["class", "polyline-class"], "svg4")
let svg5 = Group ([svg1; svg2; svg3; svg4], ["class", "group-class"], "svg5")
let svg6 = Link ("/link", [svg1; svg2; svg3; svg4], ["class", "link-class"], "svg6")

let groupSVGTests = 
    [
        "empty",
            ([], "", []),
                Group ([], [], "")
        "test props and title",
            ([("foo1", "bar1"); ("foo2", "bar2")], "title", []),
                Group ([], [("foo1", "bar1"); ("foo2", "bar2")], "title")
        "simple 1",
            ([("foo1", "bar1"); ("foo2", "bar2")], "title", [svg1]),
                Group ([svg1], [("foo1", "bar1"); ("foo2", "bar2")], "title")
        "simple 4",
            ([("foo1", "bar1"); ("foo2", "bar2")], "title", [svg1; svg2; svg3; svg4]),
                Group ([svg1; svg2; svg3; svg4], [("foo1", "bar1"); ("foo2", "bar2")], "title")
        "simple 6",
            ([("foo1", "bar1"); ("foo2", "bar2")], "title", [svg1; svg2; svg3; svg4; svg5; svg6]),
                Group ([svg1; svg2; svg3; svg4; svg5; svg6], [("foo1", "bar1"); ("foo2", "bar2")], "title")
    ]

let linkSVGTests = 
    [
        "empty",
            ("", [], "", []),
                Link ("", [], [], "")
        "test href, props and title",
            ("/link", [("foo1", "bar1"); ("foo2", "bar2")], "title", []),
                Link ("/link", [], [("foo1", "bar1"); ("foo2", "bar2")], "title")
        "simple 1",
            ("/link", [("foo1", "bar1"); ("foo2", "bar2")], "title", [svg1]),
                Link ("/link", [svg1], [("foo1", "bar1"); ("foo2", "bar2")], "title")
        "simple 4",
            ("/link", [("foo1", "bar1"); ("foo2", "bar2")], "title", [svg1; svg2; svg3; svg4]),
                Link ("/link", [svg1; svg2; svg3; svg4], [("foo1", "bar1"); ("foo2", "bar2")], "title")
        "simple 6",
            ("/link", [("foo1", "bar1"); ("foo2", "bar2")], "title", [svg1; svg2; svg3; svg4; svg5; svg6]),
                Link ("/link", [svg1; svg2; svg3; svg4; svg5; svg6], [("foo1", "bar1"); ("foo2", "bar2")], "title")
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
            "",
                ""
        "simple ",
            "foo",
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
    let svg1nc = Circle ((1., 2.), 3., [], "svg1")
    let svg2nc = Rectangle ((1., 2.), (3.,4.), [], "svg2")
    let svg3nc = Text ((1., 2.), "svg3text", [], "svg3")
    let svg4nc = Polyline ([(0.,0.); (1.,2.); (3.,4.)], [], "svg4")
    let svg5nc = Group ([svg1; svg2; svg3; svg4], [], "svg5")
    let svg6nc = Link ("/link", [svg1; svg2; svg3; svg4], [], "svg6")

    let svg1nt = Circle ((1., 2.), 3., ["class", "circ-class"], "")
    let svg2nt = Rectangle ((1., 2.), (3.,4.), ["class", "rect-class"], "")
    let svg3nt = Text ((1., 2.), "svg3text", ["class", "text-class"], "")
    let svg4nt = Polyline ([(0.,0.); (1.,2.); (3.,4.)], ["class", "polyline-class"], "")
    let svg5nt = Group ([svg1; svg2; svg3; svg4], ["class", "group-class"], "")
    let svg6nt = Link ("/link", [svg1; svg2; svg3; svg4], ["class", "link-class"], "")

    let emptyGroupChildren = Group ([], ["class", "empty-group-class"], "")
    let emptyLinkChildren = Link ("/link", [], ["class", "empty-link-class"], "")

    let emptyLink = Link ("", [svg1; svg2; svg3; svg4], ["class", "link-class"], "")
    let emptyText = Text ((1., 2.), "", ["class", "text-class"], "svg3")

    let svg1ManyProps = Circle ((1., 2.), 3., [("foo1", "bar1"); ("foo2", "bar2"); ("foo3", "bar3")], "svg1")
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

let getGridTests =
    [
        "basic1",
            ((0., 0.), (2., 2.)),
                Group ([Circle ((0.0, 0.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((0.0, 1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((0.0, 2.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((1.0, 0.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((1.0, 1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((1.0, 2.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((2.0, 0.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((2.0, 1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((2.0, 2.0),0.125,[("style", "fill: #E0E0E0;")],"")], [], "")
        "basic2",
            ((-1., -1.), (1., 1.)),
                Group ([Circle ((-1.0, -1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((-1.0, 0.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((-1.0, 1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((0.0, -1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((0.0, 0.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((0.0, 1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((1.0, -1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((1.0, 0.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((1.0, 1.0),0.125,[("style", "fill: #E0E0E0;")],"")], [], "")
        "basic3",
            ((-1., -0.), (2., 1.)),
                Group ([Circle ((-1.0, 0.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((-1.0, 1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((0.0, 0.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((0.0, 1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((1.0, 0.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((1.0, 1.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((2.0, 0.0),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((2.0, 1.0),0.125,[("style", "fill: #E0E0E0;")],"")], [], "")
        "basic4",
            ((-1.5, -1.5), (0.5, 0.5)),
                Group ([Circle ((-1.5, -1.5),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((-1.5, -0.5),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((-1.5, 0.5),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((-0.5, -1.5),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((-0.5, -0.5),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((-0.5, 0.5),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((0.5, -1.5),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((0.5, -0.5),0.125,[("style", "fill: #E0E0E0;")],"");
                Circle ((0.5, 0.5),0.125,[("style", "fill: #E0E0E0;")],"")], [], "")
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