#!/usr/bin/env python3
"""Read and print values from a Yoctopuce light sensor.

Requires the official library:  pip install yoctopuce

Usage:
    python yocto_light.py                 # first light sensor found on USB
    python yocto_light.py LIGHTMK3-XXXXX  # target a sensor by hardware/logical name
"""

import sys
import time

from yoctopuce.yocto_api import YAPI, YRefParam
from yoctopuce.yocto_lightsensor import YLightSensor


def main():
    # Talk to sensors plugged into local USB. Use an IP here instead
    # (e.g. "192.168.1.20") to reach a sensor behind a YoctoHub.
    errmsg = YRefParam()
    if YAPI.RegisterHub("usb", errmsg) != YAPI.SUCCESS:
        sys.exit("Init error: " + errmsg.value)

    # Pick a sensor: by name if one was given, else the first one found.
    if len(sys.argv) > 1:
        sensor = YLightSensor.FindLightSensor(sys.argv[1] + ".lightSensor")
    else:
        sensor = YLightSensor.FirstLightSensor()

    if sensor is None or not sensor.isOnline():
        sys.exit("No light sensor found (is it plugged in?)")

    name = sensor.get_friendlyName()
    unit = sensor.get_unit()
    print(f"Reading from {name}  (Ctrl+C to stop)\n")

    try:
        while True:
            if not sensor.isOnline():
                print("Sensor went offline.")
                break
            value = sensor.get_currentValue()
            print(f"{time.strftime('%H:%M:%S')}  {value:8.2f} {unit}")
            YAPI.Sleep(1000, errmsg)  # 1 s between reads
    except KeyboardInterrupt:
        print("\nStopped.")
    finally:
        YAPI.FreeAPI()


if __name__ == "__main__":
    main()
