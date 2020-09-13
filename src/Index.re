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
external createObjectURL: blob => url = "createObjectURL";

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

            let () = document##getElementById("save-svg")##setAttribute("href", svgBlobUrl);

            let image = createImage();
            image.onload = () => {
                let canvas = document##createElement("canvas");

                canvas.width = width;
                canvas.height = height;

                let context = canvas##getContext("2d");
                let () = context##drawImage(image, 0, 0, width, height);

                let pngUrl = canvas##toDataURL("image/png");
                let () = document##getElementById("save-png")##setAttribute("href", pngUrl);
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

        <div style={ReactDOM.Style.make(
                ~position="absolute",
                ~width="100%",
                ~height="100%",
                ~display="grid",
                ~gridTemplateRows="75% 20% 5%",
                ~gridTemplateColumns="50% 50%",
                ()
        )}>
            <div style={ReactDOM.Style.make(
                    ~gridRow="1",
                    ~gridColumn="span 2",
                    ()
            )}>
                <MCCode config bits />
            </div>
            <div style={ReactDOM.Style.make(
                    ~gridRow="2",
                    ~gridColumn="span 2",
                    ()
            )}>
                <textarea
                    value={data}
                    onChange={handleChange}
                    style={ReactDOM.Style.make(
                            ~boxSizing="border-box",
                            ~borderWidth="3px",
                            ~width="100%",
                            ~height="100%",
                            ~fontSize="3vh",
                            ()
                    )}
                />
            </div>
            <div style={ReactDOM.Style.make(
                    ~fontSize="3vh",
                    ~gridRow="3",
                    ~gridColumn="1",
                    ()
            )}>
                <a id="save-svg" download="mccode.svg">{React.string("Save as SVG")}</a>
            </div>
            <div style={ReactDOM.Style.make(
                    ~fontSize="3vh",
                    ~gridRow="3",
                    ~gridColumn="2",
                    ()
            )}>
                <a id="save-png" download="mccode.png">{React.string("Save as PNG")}</a>
            </div>
        </div>
    }
}

ReactDOMRe.render(
    <Application />,
    document##getElementById("application"),
);
