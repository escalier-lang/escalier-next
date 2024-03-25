import {useMemo, useRef} from "react";
import Editor from '@monaco-editor/react';
import {loadPlugin, typecheck} from "./typecheck";

type Props = {};

const initialSrc: string = `
let x = 5;
let y = 10;
`;

export default function Playground(props: Props) {
    const srcEditorRef = useRef<any>(null);
    const jsEditorRef = useRef<any>(null);
    const pluginPromise = useMemo(() => loadPlugin(), []);
    
    return <>
        <h1>Escalier Playground</h1>
        <div style={{flexGrow: 1, display: "flex", flexDirection: "row"}}>
            <Editor defaultLanguage="typescript" 
                    defaultValue={initialSrc} theme="vs-dark" width="50%"
                    height="calc(100vh - 80px)"
                    onMount={async (editor, monaco) => {
                        srcEditorRef.current = editor;

                        const plugin= await pluginPromise;
                        const data = await typecheck(initialSrc, plugin);
                        jsEditorRef.current?.setValue(data.js);
                    }}
                    onChange={async (value, event) => {
                        const plugin= await pluginPromise;
                        if (value) {
                            const data = await typecheck(value, plugin);
                            jsEditorRef.current?.setValue(data.js);
                        }
                    }}
            />
            <Editor defaultLanguage="javascript" 
                    defaultValue="" theme="vs-dark" width="50%"
                    height="calc(100vh - 80px)" 
                    onMount={(editor, monaco) => jsEditorRef.current = editor}
            />
        </div>
    </>;
}