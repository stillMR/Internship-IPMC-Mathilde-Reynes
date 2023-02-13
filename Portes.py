#Add Phidgets Library
from Phidget22.Phidget import *
from Phidget22.Devices.RCServo import *
#Required for sleep statement
import time

#TO KNOW
#105=porte ouverte
#165=porte fermee

#Create
servoController = RCServo()

#Open
servoController.openWaitForAttachment(1000)
pos = servoController.getPosition()
print(str(pos))

if servoController.getPosition() == 105 :
    servoController.setTargetPosition(170)
    servoController.setSpeedRampingState(True)
    servoController.setVelocityLimit(1)
    servoController.setEngaged(True)
elif servoController.getPosition() == 170 :
    servoController.setTargetPosition(105)
    servoController.setSpeedRampingState(True)
    servoController.setVelocityLimit(1)
    servoController.setEngaged(True)
else:
    print("error")


time.sleep(1.0)