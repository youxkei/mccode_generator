open Belt.Array;

[@bs.val] external document: Js.t({..}) = "document";

type encoder;
[@bs.new] external createEncoder: unit => encoder = "TextEncoder";
[@bs.send] external encode: encoder => string => array(int) = "encode";
let encoder = createEncoder();

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

        let color = "black";

        let config = MCCode.Config{
            density: 9,
            layerWidth: 32.0,
            frameWidth: 10.0,
            strokeWidth: 3.0,
            vertexWidthRatio: 1.0,
            lineDistance: 5.0,
            color,
        };


        let bits = encoder->encode(data)->byteArrayToBitArray;

        <div style={
            ReactDOM.Style.make()
                ->unsafeAddProp("position", "absolute")
                ->unsafeAddProp("width", "100%")
                ->unsafeAddProp("height", "100%")
                ->unsafeAddProp("display", "grid")
                ->unsafeAddProp("grid-template-rows", "80% 20%")
                ->unsafeAddProp("grid-template-columns", "auto")
            }
        >
            <div style={ReactDOM.Style.make()
                            ->unsafeAddProp("grid-row", "1 / 2")
                            ->unsafeAddProp("grid-column", "1 / 1")}>
                <MCCode config bits />
            </div>
            <div style={ReactDOM.Style.make()
                            ->unsafeAddProp("grid-row", "2 / 2")
                            ->unsafeAddProp("grid-column", "1 / 1")}>
                <textarea
                    value={data}
                    onChange={handleChange}
                    style={
                        ReactDOM.Style.make()
                            ->unsafeAddProp("box-sizing", "border-box")
                            ->unsafeAddProp("border-width", "3px")
                            ->unsafeAddProp("width", "100%")
                            ->unsafeAddProp("height", "100%")
                            ->unsafeAddProp("font-size", "3vw")
                    }
                />
            </div>
        </div>
    }
}

ReactDOMRe.render(
    <Application />,
    document##body,
);
