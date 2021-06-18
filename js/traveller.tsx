import React from 'react';
import { HydraAdmin } from "@api-platform/admin";

interface ComponentsForRel {
    [name: string]: React.Component;
    default: React.Component;
}

interface TravellerProps {
    entrypoint: string;
    components?: ComponentsForRel;
}

const Traveller = (props: TravellerProps) => {
    return (
      <HydraAdmin entrypoint={props.entrypoint} />
    );
}

export default Traveller;
