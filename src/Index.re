open Belt.Array;

[@bs.val] external document: Js.t({..}) = "document";

type encoder;
[@bs.new] external createEncoder: unit => encoder = "TextEncoder";
[@bs.send] external encode: encoder => string => array(int) = "encode";
let encoder = createEncoder();

type blob;
[@bs.new] external createBlob: (array(string), {. "type": string }) => blob = "Blob";

type url;
[@bs.val] [@bs.scope "URL"]
external createObjectURL: blob => url;

type image = {
    mutable onload: unit => unit,
    mutable src: url,
};
[@bs.new] external createImage: unit => image = "Image";

type size = {
    mutable width: int,
    mutable height: int,
};


let unsafeAddProp = ReactDOM.Style.unsafeAddProp;

let bit = i => if (i > 0) 1 else 0;

let byteArrayToBitArray = byteArray => {
    byteArray->map(byte => {
        [|
            bit(byte land 0b10000000),
            bit(byte land 0b01000000),
            bit(byte land 0b00100000),
            bit(byte land 0b00010000),
            bit(byte land 0b00001000),
            bit(byte land 0b00000100),
            bit(byte land 0b00000010),
            bit(byte land 0b00000001),
        |]
    })->concatMany
};

module Application {
    [@react.component]
    let make = () => {
        let (data, setData) = React.useState(() => "Hello, world!");

        let handleChange = React.useCallback(event => {
            setData(ReactEvent.Form.target(event)##value);
        });

        React.useEffect(() => {
            let svgElement = document##getElementById("mccode");
            let {width, height} = svgElement##getBBox()

            let svgString = svgElement##outerHTML;
            let svgBlobUrl = createBlob([|svgString|], { "type": "image/svg+xml" })->createObjectURL;

            document##getElementById("save-svg")##setAttribute("href", svgBlobUrl);

            let image = createImage();
            image.onload = () => {
                let canvas = document##createElement("canvas");

                canvas.width = width;
                canvas.height = height;

                let context = canvas##getContext("2d");
                context##drawImage(image, 0, 0, width, height);

                let pngUrl = canvas##toDataURL("image/png");


                document##getElementById("save-png")##setAttribute("href", pngUrl);
            };
            image.src = svgBlobUrl;

            None
        });

        let config = MCCode.Config{
            density: 9,
            layerWidth: 32.0,
            frameWidth: 10.0,
            strokeWidth: 3.0,
            vertexWidthRatio: 1.0,
            lineDistance: 5.0,
            slideBridge: true,
        };


        let bytes = encoder->encode(data);
        let bits = bytes->byteArrayToBitArray;

        <div style={
            ReactDOM.Style.make()
                ->unsafeAddProp("position", "absolute")
                ->unsafeAddProp("width", "100%")
                ->unsafeAddProp("height", "100%")
                ->unsafeAddProp("display", "grid")
                ->unsafeAddProp("grid-template-rows", "75% 20% 5%")
                ->unsafeAddProp("grid-template-columns", "50% 50%")
            }
        >
            <div style={ReactDOM.Style.make()
                            ->unsafeAddProp("grid-row", "1")
                            ->unsafeAddProp("grid-column", "span 2")}>

                <MCCode config bits />
            </div>
            <div style={ReactDOM.Style.make()
                            ->unsafeAddProp("grid-row", "2")
                            ->unsafeAddProp("grid-column", "span 2")}>
                <textarea
                    value={data}
                    onChange={handleChange}
                    style={
                        ReactDOM.Style.make()
                            ->unsafeAddProp("box-sizing", "border-box")
                            ->unsafeAddProp("border-width", "3px")
                            ->unsafeAddProp("width", "100%")
                            ->unsafeAddProp("height", "100%")
                            ->unsafeAddProp("font-size", "3vh")
                    }
                />
            </div>
            <div style={ReactDOM.Style.make()
                            ->unsafeAddProp("font-size", "3vh")
                            ->unsafeAddProp("grid-row", "3")
                            ->unsafeAddProp("grid-column", "1")}>
                <a id="save-svg" download="mccode.svg">{React.string("Save as SVG")}</a>
            </div>
            <div style={ReactDOM.Style.make()
                            ->unsafeAddProp("font-size", "3vh")
                            ->unsafeAddProp("grid-row", "3")
                            ->unsafeAddProp("grid-column", "2")}>
                <a id="save-png" download="mccode.png">{React.string("Save as PNG")}</a>
            </div>
        </div>
    }
}

module Test {
    [@react.component]
    let make = () => {
        let layerWidth = 32.0;
        let frameWidth = 10.0;

        let config = MCCode.Config{
            density: 9,
            layerWidth,
            frameWidth,
            strokeWidth: 3.0,
            vertexWidthRatio: 1.0,
            lineDistance: 5.0,
            slideBridge: false,
        };

        let radius = layerWidth *. 4.0 +. frameWidth;
        let width = radius *. 2.0;

        let bits = makeBy(100, (_) => 1);

        let dots = <>
            <circle x="0" y="0" r="2" transform="translate(40, 0)" stroke="none" />
            <circle x="0" y="0" r="2" transform="translate(50, 0)" stroke="none" />
            <circle x="0" y="0" r="2" transform="translate(60, 0)" stroke="none" />
            <circle x="0" y="0" r="2" transform="translate(70, 0)" stroke="none" />
            <circle x="0" y="0" r="2" transform="translate(80, 0)" stroke="none" />
        </>;

        <div style={
            ReactDOM.Style.make()
                ->unsafeAddProp("position", "absolute")
                ->unsafeAddProp("width", "100%")
                ->unsafeAddProp("height", "100%")
                ->unsafeAddProp("display", "grid")
                ->unsafeAddProp("grid-template-rows", "100%")
                ->unsafeAddProp("grid-template-columns", "auto")
            }
        >
            <div style={ReactDOM.Style.make()
                            ->unsafeAddProp("grid-row", "1 / 1")
                            ->unsafeAddProp("grid-column", "1 / 1")}>

                <svg
                    width="100%"
                    height="100%"
                    viewBox={{j|-$radius -$radius $width $width|j}}
                >
                    {MCCode.makeLayer(3, bits, config)}
                    {MCCode.makeLayer(4, bits, config)}
                    {range(0, 15)
                        ->map(i => {
                            let degree = float_of_int(i) *. (360.0 /. 16.0);
                            <g transform={j|rotate($degree)|j}>{dots}</g>
                        })->React.array}
                </svg>
            </div>
        </div>
    }
}

ReactDOMRe.render(
    <Application />,
    document##body,
);
