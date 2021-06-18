import Traveller from "./traveller";
import React from "react";
import ReactDOM from "react-dom";

const app = <Traveller entrypoint="/"/> ;

ReactDOM.render(
    app,
    document.getElementById("root"),
);

