module Hue.Adapter where

import Hue.Process

type HueAdapter context msg = (context -> msg -> IO ()) -> IO Task
