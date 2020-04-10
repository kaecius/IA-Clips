#!/usr/bin/env python3

import clips

clips.Load("./clips/peces.pont")
clips.Load("./clips/peces.pins")
clips.Load("./clips/peces.clp")
clips.Reset()
i = 0
while not input():
    print(f"Ciclo {i}")
    print("Instancias")
    instance = clips.InitialInstance()
    while (True):
        instance = instance.Next()
        if (not instance):
            break
        print(instance.PPForm())
    print("Agenda")
    clips.RefreshAgenda()
    clips.PrintAgenda()
    print()
    clips.SendCommand("(run 1)")
    i+=1
