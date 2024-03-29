EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 2 2
Title "Z79Forth Reference Board"
Date "2022-03-14"
Rev "2.2.2"
Comp "Francois Laagel"
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Max232:Max232IN U13
U 1 1 5FEC7974
P 4600 3700
F 0 "U13" V 4550 3600 50  0000 C CNN
F 1 "Max232EPE+" V 4650 3600 50  0000 C CNN
F 2 "" H 4600 3700 50  0001 C CNN
F 3 "https://www.ti.com/lit/ds/symlink/max232.pdf" H 4600 3700 50  0001 C CNN
	1    4600 3700
	1    0    0    -1  
$EndComp
$Comp
L Jumper:Jumper_3_Bridged12 JP3
U 1 1 5FED1E9E
P 2900 4900
F 0 "JP3" H 2900 4700 50  0000 C CNN
F 1 "RS232" H 2750 4800 50  0000 C CNN
F 2 "" H 2900 4900 50  0001 C CNN
F 3 "~" H 2900 4900 50  0001 C CNN
	1    2900 4900
	1    0    0    -1  
$EndComp
$Comp
L Jumper:Jumper_3_Bridged12 JP4
U 1 1 5FEF9A1F
P 2900 5550
F 0 "JP4" H 2900 5350 50  0000 C CNN
F 1 "RS232" H 2700 5450 50  0000 C CNN
F 2 "" H 2900 5550 50  0001 C CNN
F 3 "~" H 2900 5550 50  0001 C CNN
	1    2900 5550
	1    0    0    -1  
$EndComp
$Comp
L Jumper:Jumper_3_Bridged12 JP5
U 1 1 5FEFEE3D
P 2900 6150
F 0 "JP5" H 2900 5950 50  0000 C CNN
F 1 "RS232" H 2700 6050 50  0000 C CNN
F 2 "" H 2900 6150 50  0001 C CNN
F 3 "~" H 2900 6150 50  0001 C CNN
	1    2900 6150
	1    0    0    -1  
$EndComp
$Comp
L Device:C C6
U 1 1 5FEE2CA2
P 5900 3250
F 0 "C6" H 6015 3296 50  0000 L CNN
F 1 "1 uF" H 6015 3205 50  0000 L CNN
F 2 "" H 5938 3100 50  0001 C CNN
F 3 "~" H 5900 3250 50  0001 C CNN
	1    5900 3250
	1    0    0    -1  
$EndComp
Wire Wire Line
	4600 3100 4600 3000
Connection ~ 4600 3100
$Comp
L Device:C C8
U 1 1 5FEE7527
P 5700 3600
F 0 "C8" V 5850 3600 50  0000 C CNN
F 1 "1 uF" V 5550 3600 50  0000 C CNN
F 2 "" H 5738 3450 50  0001 C CNN
F 3 "~" H 5700 3600 50  0001 C CNN
	1    5700 3600
	0    -1   -1   0   
$EndComp
$Comp
L Device:C C10
U 1 1 5FEE865B
P 5250 3800
F 0 "C10" V 5400 3800 50  0000 C CNN
F 1 "1 uF" V 5100 3800 50  0000 C CNN
F 2 "" H 5288 3650 50  0001 C CNN
F 3 "~" H 5250 3800 50  0001 C CNN
	1    5250 3800
	0    -1   -1   0   
$EndComp
$Comp
L power:GND #PWR0148
U 1 1 5FEEF06B
P 6150 3750
F 0 "#PWR0148" H 6150 3500 50  0001 C CNN
F 1 "GND" H 6155 3577 50  0000 C CNN
F 2 "" H 6150 3750 50  0001 C CNN
F 3 "" H 6150 3750 50  0001 C CNN
	1    6150 3750
	1    0    0    -1  
$EndComp
Wire Wire Line
	5100 3600 5550 3600
Wire Wire Line
	5850 3600 5900 3600
$Comp
L Device:C C7
U 1 1 5FEF2C2E
P 3850 3400
F 0 "C7" V 4000 3400 50  0000 C CNN
F 1 "1 uF" V 3750 3250 50  0000 C CNN
F 2 "" H 3888 3250 50  0001 C CNN
F 3 "~" H 3850 3400 50  0001 C CNN
	1    3850 3400
	0    -1   -1   0   
$EndComp
Wire Wire Line
	4000 3400 4100 3400
Wire Wire Line
	3700 3400 3700 3600
Wire Wire Line
	3700 3600 4100 3600
Wire Wire Line
	4600 3100 5900 3100
Wire Wire Line
	5100 3400 5900 3400
Wire Wire Line
	5900 3400 5900 3600
Connection ~ 5900 3400
Connection ~ 5900 3600
$Comp
L Device:C C9
U 1 1 5FF02157
P 3850 3800
F 0 "C9" V 4000 3800 50  0000 C CNN
F 1 "1 uF" V 3750 3650 50  0000 C CNN
F 2 "" H 3888 3650 50  0001 C CNN
F 3 "~" H 3850 3800 50  0001 C CNN
	1    3850 3800
	0    -1   -1   0   
$EndComp
Wire Wire Line
	4000 3800 4100 3800
Wire Wire Line
	3700 3800 3700 4000
Wire Wire Line
	3700 4000 4100 4000
Wire Wire Line
	5900 3600 5900 3800
Wire Wire Line
	5400 3800 5900 3800
Wire Wire Line
	5900 3600 6150 3600
Wire Wire Line
	6150 3600 6150 3750
Text GLabel 1350 6050 0    36   Output ~ 0
~FIRQ
Wire Wire Line
	1850 6600 1850 6650
Wire Wire Line
	1850 6600 2000 6600
Connection ~ 1850 6600
Wire Wire Line
	1250 6600 1850 6600
$Comp
L power:GND #PWR0133
U 1 1 61475748
P 1850 6650
F 0 "#PWR0133" H 1850 6400 50  0001 C CNN
F 1 "GND" H 1855 6477 50  0000 C CNN
F 2 "" H 1850 6650 50  0001 C CNN
F 3 "" H 1850 6650 50  0001 C CNN
	1    1850 6650
	1    0    0    -1  
$EndComp
Text GLabel 2000 6600 2    50   Output ~ 0
U9GND
Text GLabel 1700 4050 0    50   Output ~ 0
U9VCC
Text GLabel 1350 4950 0    50   BiDi ~ 0
D7
Text GLabel 1350 4850 0    50   BiDi ~ 0
D6
Text GLabel 1350 4750 0    50   BiDi ~ 0
D5
Text GLabel 1350 4650 0    50   BiDi ~ 0
D4
Text GLabel 1350 4550 0    50   BiDi ~ 0
D3
Text GLabel 1350 4450 0    50   BiDi ~ 0
D2
Text GLabel 1350 4350 0    50   BiDi ~ 0
D1
Text GLabel 1350 4250 0    50   BiDi ~ 0
D0
Wire Wire Line
	1250 6250 1350 6250
Text GLabel 1100 5850 0    50   Input ~ 0
SERCLK
Text GLabel 1350 5600 0    50   Input ~ 0
ACIAE
$Comp
L power:VCC #PWR0134
U 1 1 61475725
P 1050 5400
F 0 "#PWR0134" H 1050 5250 50  0001 C CNN
F 1 "VCC" H 1065 5573 50  0000 C CNN
F 2 "" H 1050 5400 50  0001 C CNN
F 3 "" H 1050 5400 50  0001 C CNN
	1    1050 5400
	1    0    0    -1  
$EndComp
Connection ~ 1250 5500
Wire Wire Line
	1250 5500 1350 5500
Wire Wire Line
	1250 5400 1250 5500
Wire Wire Line
	1350 5400 1250 5400
Text GLabel 1350 5300 0    39   Input ~ 0
~DEV6
Text GLabel 1350 5200 0    50   Input ~ 0
A0
Text GLabel 1350 5100 0    39   Input ~ 0
~WR
$Comp
L power:VCC #PWR0135
U 1 1 61475717
P 1850 4050
F 0 "#PWR0135" H 1850 3900 50  0001 C CNN
F 1 "VCC" H 1865 4223 50  0000 C CNN
F 2 "" H 1850 4050 50  0001 C CNN
F 3 "" H 1850 4050 50  0001 C CNN
	1    1850 4050
	1    0    0    -1  
$EndComp
$Comp
L Z79Forth:IDC10-Male J3
U 1 1 6157E208
P 5750 4300
F 0 "J3" H 5800 4000 50  0000 C CNN
F 1 "IDC10-Male" H 5850 3800 50  0000 C CNN
F 2 "" H 5750 4300 50  0001 C CNN
F 3 "~" H 5750 4300 50  0001 C CNN
	1    5750 4300
	1    0    0    -1  
$EndComp
NoConn ~ 5550 4100
NoConn ~ 6050 4200
NoConn ~ 6050 4300
NoConn ~ 5550 4500
NoConn ~ 6050 4500
Wire Wire Line
	5100 4200 5550 4200
Wire Wire Line
	5100 4300 5200 4300
Wire Wire Line
	5200 4300 5200 4400
Wire Wire Line
	5200 4400 5550 4400
Wire Wire Line
	5100 4000 6050 4000
Wire Wire Line
	6050 4000 6050 4100
Wire Wire Line
	5100 4500 5100 4700
Wire Wire Line
	5100 4700 6200 4700
Wire Wire Line
	6200 4700 6200 4400
Wire Wire Line
	6200 4400 6050 4400
$Comp
L power:VCC #PWR0140
U 1 1 615AB1EA
P 4600 3000
F 0 "#PWR0140" H 4600 2850 50  0001 C CNN
F 1 "VCC" H 4615 3173 50  0000 C CNN
F 2 "" H 4600 3000 50  0001 C CNN
F 3 "" H 4600 3000 50  0001 C CNN
	1    4600 3000
	1    0    0    -1  
$EndComp
Text Label 5650 4950 0    50   ~ 0
RS232DTE
$Comp
L power:GND #PWR0144
U 1 1 615EEA2E
P 5350 4450
F 0 "#PWR0144" H 5350 4200 50  0001 C CNN
F 1 "GND" H 5355 4277 50  0000 C CNN
F 2 "" H 5350 4450 50  0001 C CNN
F 3 "" H 5350 4450 50  0001 C CNN
	1    5350 4450
	1    0    0    -1  
$EndComp
$Comp
L Z79Forth:HD63B50 U9
U 1 1 61475711
P 1850 5350
F 0 "U9" V 1750 5350 50  0000 C CNN
F 1 "HD63B50" V 1850 5350 50  0000 C CNN
F 2 "Package_DIP:DIP-40_W15.24mm" H 1900 4200 50  0001 L CNN
F 3 "http://pdf.datasheetcatalog.com/datasheet/motorola/MC6850.pdf" H 1850 5350 50  0001 C CNN
	1    1850 5350
	1    0    0    -1  
$EndComp
Wire Wire Line
	1250 6250 1250 6600
Wire Wire Line
	5350 4300 5550 4300
$Comp
L Jumper:Jumper_3_Bridged12 JP2
U 1 1 5FECF1D6
P 2900 4250
F 0 "JP2" H 2900 4050 50  0000 C CNN
F 1 "RS232" H 2750 4150 50  0000 C CNN
F 2 "" H 2900 4250 50  0001 C CNN
F 3 "~" H 2900 4250 50  0001 C CNN
	1    2900 4250
	1    0    0    -1  
$EndComp
Wire Wire Line
	1050 5500 1250 5500
Wire Wire Line
	1050 5500 1050 5400
Wire Wire Line
	2250 4400 2900 4400
Text GLabel 3150 4250 2    50   Output ~ 0
URXD
Text GLabel 3150 4150 2    50   Output ~ 0
RTXD
Text GLabel 4100 4200 0    50   Input ~ 0
RTXD
Wire Wire Line
	2250 5050 2900 5050
Wire Wire Line
	3150 4150 2650 4150
Text GLabel 3150 4900 2    39   Output ~ 0
~UCTS
Text GLabel 3150 4800 2    39   Output ~ 0
~RRTS
Wire Wire Line
	3150 4800 2650 4800
Wire Wire Line
	2650 4800 2650 4900
Text GLabel 3150 5550 2    50   Input ~ 0
UTXD
Text GLabel 3150 5450 2    50   Input ~ 0
RRXD
Wire Wire Line
	3150 5450 2650 5450
Wire Wire Line
	2650 5450 2650 5550
Text GLabel 3150 6150 2    39   Input ~ 0
~URTS
Text GLabel 3150 6050 2    39   Input ~ 0
~RCTS
Wire Wire Line
	3150 6050 2650 6050
Wire Wire Line
	2650 6050 2650 6150
Text GLabel 4100 4300 0    39   Input ~ 0
~RRTS
Text GLabel 4100 4400 0    50   Output ~ 0
RRXD
Text GLabel 4100 4500 0    39   Output ~ 0
~RCTS
Wire Wire Line
	1850 4050 1700 4050
Connection ~ 1850 4050
Wire Wire Line
	2650 4250 2650 4150
Text Label 3100 4100 2    50   ~ 0
USB
Text Label 3100 4750 2    50   ~ 0
USB
Text Label 3100 5400 2    50   ~ 0
USB
Text Label 3100 6000 2    50   ~ 0
USB
Wire Wire Line
	1850 6550 1850 6600
Wire Wire Line
	5350 4300 5350 4450
Connection ~ 4600 6900
Wire Wire Line
	4600 6900 5100 6900
Connection ~ 4600 5500
Wire Wire Line
	4600 5500 5100 5500
$Comp
L power:PWR_FLAG #FLG0101
U 1 1 61579FF6
P 5350 5500
F 0 "#FLG0101" H 5350 5575 50  0001 C CNN
F 1 "PWR_FLAG" H 5350 5673 50  0000 C CNN
F 2 "" H 5350 5500 50  0001 C CNN
F 3 "~" H 5350 5500 50  0001 C CNN
	1    5350 5500
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0142
U 1 1 61579FFC
P 4600 6900
F 0 "#PWR0142" H 4600 6650 50  0001 C CNN
F 1 "GND" H 4605 6727 50  0000 C CNN
F 2 "" H 4600 6900 50  0001 C CNN
F 3 "" H 4600 6900 50  0001 C CNN
	1    4600 6900
	1    0    0    -1  
$EndComp
$Comp
L power:VCC #PWR0143
U 1 1 6157A002
P 4600 5500
F 0 "#PWR0143" H 4600 5350 50  0001 C CNN
F 1 "VCC" H 4615 5673 50  0000 C CNN
F 2 "" H 4600 5500 50  0001 C CNN
F 3 "" H 4600 5500 50  0001 C CNN
	1    4600 5500
	1    0    0    -1  
$EndComp
Text Label 4800 6400 1    50   ~ 0
USBslave
$Comp
L Z79Forth:FT232RL-Breakout J2
U 1 1 6157A009
P 4650 6200
F 0 "J2" V 4450 6100 50  0000 L CNN
F 1 "FT232RL-Breakout" V 4600 5850 50  0000 L CNN
F 2 "" H 4650 6200 50  0001 C CNN
F 3 "" H 4650 6200 50  0001 C CNN
	1    4650 6200
	1    0    0    -1  
$EndComp
Text GLabel 4150 5950 0    50   Input ~ 0
URXD
Text GLabel 4150 6050 0    50   Output ~ 0
UTXD
Text GLabel 4150 5850 0    39   Input ~ 0
~UCTS
Text GLabel 4150 5750 0    39   Output ~ 0
~URTS
$Comp
L Z79Forth:Conn_2Rows-50Pins J4
U 1 1 617D77B8
P 10450 4500
F 0 "J4" H 10500 5917 50  0000 C CNN
F 1 "IDC50-Male" H 10500 5826 50  0000 C CNN
F 2 "" H 10450 4500 50  0001 C CNN
F 3 "~" H 10450 4500 50  0001 C CNN
	1    10450 4500
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0146
U 1 1 617DF73C
P 10250 3300
F 0 "#PWR0146" H 10250 3050 50  0001 C CNN
F 1 "GND" V 10255 3172 50  0000 R CNN
F 2 "" H 10250 3300 50  0001 C CNN
F 3 "" H 10250 3300 50  0001 C CNN
	1    10250 3300
	0    1    -1   0   
$EndComp
$Comp
L power:VCC #PWR0147
U 1 1 617DFB15
P 10750 3300
F 0 "#PWR0147" H 10750 3150 50  0001 C CNN
F 1 "VCC" V 10765 3428 50  0000 L CNN
F 2 "" H 10750 3300 50  0001 C CNN
F 3 "" H 10750 3300 50  0001 C CNN
	1    10750 3300
	0    1    1    0   
$EndComp
Text GLabel 10250 3400 0    50   Output ~ 0
SYSCLK
Text GLabel 10750 3400 2    36   Output ~ 0
~Q
Text GLabel 10250 3500 0    50   Output ~ 0
E
Text GLabel 10750 3500 2    36   Input ~ 0
~STRETCH
NoConn ~ 10250 5400
NoConn ~ 10750 5400
NoConn ~ 10750 5500
NoConn ~ 10750 5600
NoConn ~ 10750 5700
NoConn ~ 10250 5700
NoConn ~ 10250 5600
NoConn ~ 10250 5500
Text GLabel 10250 3600 0    36   Output ~ 0
~CFRST
Text GLabel 10750 3600 2    36   Input ~ 0
~HALT
Text GLabel 10250 3700 0    36   Input ~ 0
~NMI
Text GLabel 10750 3700 2    36   Input ~ 0
~FIRQ
Text GLabel 10250 3800 0    36   Input ~ 0
~IRQ
Text GLabel 10250 3900 0    36   Output ~ 0
~WR
Text GLabel 10750 3900 2    36   Output ~ 0
~8KB4
Text GLabel 10250 4000 0    36   Output ~ 0
~8KB5
Text GLabel 10750 4000 2    36   Output ~ 0
~DEV1
Text GLabel 10250 4100 0    36   Output ~ 0
~DEV2
Text GLabel 10250 4200 0    36   Output ~ 0
~DEV4
Text GLabel 10750 4100 2    36   Output ~ 0
~DEV3
Text GLabel 10750 4200 2    36   Output ~ 0
~DEV5
Text GLabel 10250 4300 0    36   Output ~ 0
~DEV7
Text GLabel 10750 4300 2    50   BiDi ~ 0
D0
Text GLabel 10750 4400 2    50   BiDi ~ 0
D2
Text GLabel 10750 4500 2    50   BiDi ~ 0
D4
Text GLabel 10750 4600 2    50   BiDi ~ 0
D6
Text GLabel 10250 4400 0    50   BiDi ~ 0
D1
Text GLabel 10250 4500 0    50   BiDi ~ 0
D3
Text GLabel 10250 4600 0    50   BiDi ~ 0
D5
Text GLabel 10250 4700 0    50   BiDi ~ 0
D7
Text GLabel 10750 4700 2    50   Output ~ 0
A0
Text GLabel 10750 4800 2    50   Output ~ 0
A2
Text GLabel 10750 4900 2    50   Output ~ 0
A4
Text GLabel 10750 5000 2    50   Output ~ 0
A6
Text GLabel 10750 5100 2    50   Output ~ 0
A8
Text GLabel 10750 5200 2    50   Output ~ 0
A10
Text GLabel 10750 5300 2    50   Output ~ 0
A12
Text GLabel 10250 4800 0    50   Output ~ 0
A1
Text GLabel 10250 4900 0    50   Output ~ 0
A3
Text GLabel 10250 5000 0    50   Output ~ 0
A5
Text GLabel 10250 5100 0    50   Output ~ 0
A7
Text GLabel 10250 5200 0    50   Output ~ 0
A9
Text GLabel 10250 5300 0    50   Output ~ 0
A11
Wire Wire Line
	1250 5750 1250 5850
$Comp
L power:PWR_FLAG #FLG0102
U 1 1 617DD80A
P 5350 6900
F 0 "#FLG0102" H 5350 6975 50  0001 C CNN
F 1 "PWR_FLAG" H 5350 7073 50  0000 C CNN
F 2 "" H 5350 6900 50  0001 C CNN
F 3 "~" H 5350 6900 50  0001 C CNN
	1    5350 6900
	1    0    0    -1  
$EndComp
Wire Wire Line
	2250 5700 2900 5700
Wire Wire Line
	2250 6300 2900 6300
$Comp
L Device:C C?
U 1 1 618554EF
P 1350 1600
AR Path="/618554EF" Ref="C?"  Part="1" 
AR Path="/615D6456/618554EF" Ref="C2"  Part="1" 
F 0 "C2" H 1465 1646 50  0000 L CNN
F 1 "100nF" H 1465 1555 50  0000 L CNN
F 2 "" H 1388 1450 50  0001 C CNN
F 3 "~" H 1350 1600 50  0001 C CNN
	1    1350 1600
	1    0    0    -1  
$EndComp
$Comp
L Device:C C?
U 1 1 618554F5
P 1800 1600
AR Path="/618554F5" Ref="C?"  Part="1" 
AR Path="/615D6456/618554F5" Ref="C3"  Part="1" 
F 0 "C3" H 1915 1646 50  0000 L CNN
F 1 "100nF" H 1915 1555 50  0000 L CNN
F 2 "" H 1838 1450 50  0001 C CNN
F 3 "~" H 1800 1600 50  0001 C CNN
	1    1800 1600
	1    0    0    -1  
$EndComp
$Comp
L Device:C C?
U 1 1 618554FB
P 2250 1600
AR Path="/618554FB" Ref="C?"  Part="1" 
AR Path="/615D6456/618554FB" Ref="C4"  Part="1" 
F 0 "C4" H 2365 1646 50  0000 L CNN
F 1 "100nF" H 2365 1555 50  0000 L CNN
F 2 "" H 2288 1450 50  0001 C CNN
F 3 "~" H 2250 1600 50  0001 C CNN
	1    2250 1600
	1    0    0    -1  
$EndComp
$Comp
L Device:C C?
U 1 1 61855501
P 2700 1600
AR Path="/61855501" Ref="C?"  Part="1" 
AR Path="/615D6456/61855501" Ref="C5"  Part="1" 
F 0 "C5" H 2815 1646 50  0000 L CNN
F 1 "100nF" H 2815 1555 50  0000 L CNN
F 2 "" H 2738 1450 50  0001 C CNN
F 3 "~" H 2700 1600 50  0001 C CNN
	1    2700 1600
	1    0    0    -1  
$EndComp
Text GLabel 1350 1450 1    50   Input ~ 0
U9VCC
Text GLabel 1800 1450 1    50   Input ~ 0
U10VCC
Text GLabel 2250 1450 1    50   Input ~ 0
U11VCC
Text GLabel 2700 1450 1    50   Input ~ 0
J1VCC
Text GLabel 1350 1750 3    50   Input ~ 0
U9GND
Text GLabel 1800 1750 3    50   Input ~ 0
U10GND
Text GLabel 2250 1750 3    50   Input ~ 0
U11GND
Text GLabel 2700 1750 3    50   Input ~ 0
J1GND
$Comp
L Z79Forth:74HCT02 U?
U 2 1 6185F5D1
P 5900 1950
AR Path="/6185F5D1" Ref="U?"  Part="2" 
AR Path="/615D6456/6185F5D1" Ref="U6"  Part="2" 
F 0 "U6" H 5900 2275 50  0000 C CNN
F 1 "74HCT02" H 5900 2184 50  0000 C CNN
F 2 "" H 5900 1950 50  0001 C CNN
F 3 "http://www.ti.com/lit/gpn/sn74hct02" H 5900 1950 50  0001 C CNN
	2    5900 1950
	1    0    0    -1  
$EndComp
$Comp
L Z79Forth:74HCT02 U?
U 3 1 6185F5D7
P 6850 1950
AR Path="/6185F5D7" Ref="U?"  Part="3" 
AR Path="/615D6456/6185F5D7" Ref="U6"  Part="3" 
F 0 "U6" H 6850 2275 50  0000 C CNN
F 1 "74HCT02" H 6850 2184 50  0000 C CNN
F 2 "" H 6850 1950 50  0001 C CNN
F 3 "http://www.ti.com/lit/gpn/sn74hct02" H 6850 1950 50  0001 C CNN
	3    6850 1950
	1    0    0    -1  
$EndComp
$Comp
L Z79Forth:74HCT02 U?
U 4 1 6185F5DD
P 7850 1950
AR Path="/6185F5DD" Ref="U?"  Part="4" 
AR Path="/615D6456/6185F5DD" Ref="U6"  Part="4" 
F 0 "U6" H 7850 2275 50  0000 C CNN
F 1 "74HCT02" H 7850 2184 50  0000 C CNN
F 2 "" H 7850 1950 50  0001 C CNN
F 3 "http://www.ti.com/lit/gpn/sn74hct02" H 7850 1950 50  0001 C CNN
	4    7850 1950
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR?
U 1 1 6185F5E3
P 5600 2200
AR Path="/6185F5E3" Ref="#PWR?"  Part="1" 
AR Path="/615D6456/6185F5E3" Ref="#PWR0126"  Part="1" 
F 0 "#PWR0126" H 5600 1950 50  0001 C CNN
F 1 "GND" H 5605 2027 50  0000 C CNN
F 2 "" H 5600 2200 50  0001 C CNN
F 3 "" H 5600 2200 50  0001 C CNN
	1    5600 2200
	1    0    0    -1  
$EndComp
Wire Wire Line
	5600 1850 5600 2050
Connection ~ 5600 2050
Wire Wire Line
	5600 2050 5600 2200
$Comp
L power:GND #PWR?
U 1 1 6185F5EC
P 6550 2200
AR Path="/6185F5EC" Ref="#PWR?"  Part="1" 
AR Path="/615D6456/6185F5EC" Ref="#PWR0127"  Part="1" 
F 0 "#PWR0127" H 6550 1950 50  0001 C CNN
F 1 "GND" H 6555 2027 50  0000 C CNN
F 2 "" H 6550 2200 50  0001 C CNN
F 3 "" H 6550 2200 50  0001 C CNN
	1    6550 2200
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR?
U 1 1 6185F5F2
P 7550 2200
AR Path="/6185F5F2" Ref="#PWR?"  Part="1" 
AR Path="/615D6456/6185F5F2" Ref="#PWR0132"  Part="1" 
F 0 "#PWR0132" H 7550 1950 50  0001 C CNN
F 1 "GND" H 7555 2027 50  0000 C CNN
F 2 "" H 7550 2200 50  0001 C CNN
F 3 "" H 7550 2200 50  0001 C CNN
	1    7550 2200
	1    0    0    -1  
$EndComp
Wire Wire Line
	6550 2200 6550 2050
Connection ~ 6550 2050
Wire Wire Line
	6550 2050 6550 1850
Wire Wire Line
	7550 2200 7550 2050
Connection ~ 7550 2050
Wire Wire Line
	7550 2050 7550 1850
NoConn ~ 6200 1950
NoConn ~ 7150 1950
NoConn ~ 8150 1950
Wire Wire Line
	5100 4000 5100 4400
$Comp
L Z79Forth:MC146818P U15
U 1 1 6199D951
P 8900 3800
F 0 "U15" V 8800 3800 50  0000 C CNN
F 1 "MC146818P" V 8900 3800 50  0000 C CNN
F 2 "Package_DIP:DIP-40_W15.24mm" H 8900 2650 50  0001 L CNN
F 3 "http://pdf.datasheetcatalog.com/datasheet/motorola/MC6850.pdf" H 8850 3800 50  0001 C CNN
	1    8900 3800
	1    0    0    -1  
$EndComp
$Comp
L Z79Forth:DS32kHz-N Y3
U 1 1 6199E7A9
P 8900 5550
F 0 "Y3" H 9000 5850 50  0000 L CNN
F 1 "DS32kHz-N" H 9050 5250 50  0000 L CNN
F 2 "Oscillator:Oscillator_DIP-14" H 9350 5200 50  0001 C CNN
F 3 "http://cdn-reichelt.de/documents/datenblatt/B400/OSZI.pdf" H 8800 5550 50  0001 C CNN
	1    8900 5550
	1    0    0    -1  
$EndComp
$Comp
L Z79Forth:ATF22V10C-7PX U14
U 1 1 6199F333
P 7400 4450
F 0 "U14" V 7300 4450 50  0000 C CNN
F 1 "ATF22V10C-7PX" V 7450 4450 50  0000 C CNN
F 2 "" H 7400 4450 50  0001 C CNN
F 3 "" H 7400 4450 50  0001 C CNN
	1    7400 4450
	1    0    0    -1  
$EndComp
NoConn ~ 6900 5050
NoConn ~ 9350 3550
NoConn ~ 9350 4200
NoConn ~ 9350 4400
NoConn ~ 8500 5550
$Comp
L power:GND #PWR0141
U 1 1 619B1966
P 7400 5250
F 0 "#PWR0141" H 7400 5000 50  0001 C CNN
F 1 "GND" H 7405 5077 50  0000 C CNN
F 2 "" H 7400 5250 50  0001 C CNN
F 3 "" H 7400 5250 50  0001 C CNN
	1    7400 5250
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0149
U 1 1 619B22F0
P 8900 5850
F 0 "#PWR0149" H 8900 5600 50  0001 C CNN
F 1 "GND" H 8905 5677 50  0000 C CNN
F 2 "" H 8900 5850 50  0001 C CNN
F 3 "" H 8900 5850 50  0001 C CNN
	1    8900 5850
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0150
U 1 1 619B3B93
P 8900 4550
F 0 "#PWR0150" H 8900 4300 50  0001 C CNN
F 1 "GND" H 8905 4377 50  0000 C CNN
F 2 "" H 8900 4550 50  0001 C CNN
F 3 "" H 8900 4550 50  0001 C CNN
	1    8900 4550
	1    0    0    -1  
$EndComp
$Comp
L power:VCC #PWR0151
U 1 1 619B44B0
P 7400 3650
F 0 "#PWR0151" H 7400 3500 50  0001 C CNN
F 1 "VCC" H 7415 3823 50  0000 C CNN
F 2 "" H 7400 3650 50  0001 C CNN
F 3 "" H 7400 3650 50  0001 C CNN
	1    7400 3650
	1    0    0    -1  
$EndComp
$Comp
L power:VCC #PWR0152
U 1 1 619B4EC1
P 8900 3000
F 0 "#PWR0152" H 8900 2850 50  0001 C CNN
F 1 "VCC" H 8915 3173 50  0000 C CNN
F 2 "" H 8900 3000 50  0001 C CNN
F 3 "" H 8900 3000 50  0001 C CNN
	1    8900 3000
	1    0    0    -1  
$EndComp
$Comp
L power:VCC #PWR0153
U 1 1 619B5813
P 8900 5250
F 0 "#PWR0153" H 8900 5100 50  0001 C CNN
F 1 "VCC" H 8915 5423 50  0000 C CNN
F 2 "" H 8900 5250 50  0001 C CNN
F 3 "" H 8900 5250 50  0001 C CNN
	1    8900 5250
	1    0    0    -1  
$EndComp
Text GLabel 8450 3250 0    50   BiDi ~ 0
D0
Text GLabel 8450 3350 0    50   BiDi ~ 0
D1
Text GLabel 8450 3450 0    50   BiDi ~ 0
D2
Text GLabel 8450 3550 0    50   BiDi ~ 0
D3
Text GLabel 8450 3650 0    50   BiDi ~ 0
D4
Text GLabel 8450 3750 0    50   BiDi ~ 0
D5
Text GLabel 8450 3850 0    50   BiDi ~ 0
D6
Text GLabel 8450 3950 0    50   BiDi ~ 0
D7
Text GLabel 8450 4250 0    36   Input ~ 0
~WR
Text GLabel 9350 3750 2    36   Input ~ 0
~CFRST
Text GLabel 6900 4350 0    36   Input ~ 0
~WR
Text GLabel 6900 3850 0    36   Input ~ 0
~CFRST
Text GLabel 6900 4950 0    50   Input ~ 0
SYSCLK
Text GLabel 6900 3950 0    36   Input ~ 0
~Q
Text GLabel 6900 4050 0    50   Input ~ 0
E
Text GLabel 7900 4750 2    36   Output ~ 0
~STRETCH
Text GLabel 6550 4450 0    36   Input ~ 0
~DEV5
Text GLabel 6900 4550 0    50   Input ~ 0
A0
Wire Wire Line
	6550 4450 6700 4450
NoConn ~ 7900 4650
NoConn ~ 7900 4450
NoConn ~ 7900 4350
NoConn ~ 7900 4250
NoConn ~ 7900 4550
NoConn ~ 6900 4250
NoConn ~ 7900 3950
NoConn ~ 7900 3850
Wire Wire Line
	6700 4450 6700 4150
Wire Wire Line
	6700 4150 6900 4150
Connection ~ 6700 4450
Wire Wire Line
	6700 4450 6900 4450
Wire Wire Line
	7900 4050 8450 4050
Wire Wire Line
	7900 4150 8450 4150
NoConn ~ 6900 4650
NoConn ~ 6900 4750
Wire Wire Line
	9300 5550 9650 5550
Wire Wire Line
	9650 5550 9650 4300
Wire Wire Line
	9650 4300 9350 4300
Wire Wire Line
	9350 4100 9650 4100
Wire Wire Line
	9650 3000 9500 3000
Connection ~ 8900 3000
Wire Wire Line
	8450 4350 8350 4350
Wire Wire Line
	8350 4350 8350 4550
Wire Wire Line
	8350 4550 8900 4550
Connection ~ 8900 4550
Wire Wire Line
	9650 3000 9650 4100
Wire Wire Line
	9350 3250 9500 3250
Wire Wire Line
	9500 3250 9500 3000
Connection ~ 9500 3000
Wire Wire Line
	9500 3000 8900 3000
Text GLabel 9350 3850 2    36   Output ~ 0
~FIRQ
$Comp
L Device:C_Polarized C12
U 1 1 61F20B30
P 5100 6200
F 0 "C12" H 5218 6246 50  0000 L CNN
F 1 "10000 uF" H 5218 6155 50  0000 L CNN
F 2 "" H 5138 6050 50  0001 C CNN
F 3 "~" H 5100 6200 50  0001 C CNN
	1    5100 6200
	1    0    0    -1  
$EndComp
Wire Wire Line
	5100 6050 5100 5500
Connection ~ 5100 5500
Wire Wire Line
	5100 5500 5350 5500
Wire Wire Line
	5100 6350 5100 6900
Connection ~ 5100 6900
Wire Wire Line
	5100 6900 5350 6900
Text GLabel 5000 1950 2    39   Output ~ 0
~NMI
$Comp
L power:GND #PWR?
U 1 1 61855510
P 4250 2350
AR Path="/61855510" Ref="#PWR?"  Part="1" 
AR Path="/615D6456/61855510" Ref="#PWR0111"  Part="1" 
F 0 "#PWR0111" H 4250 2100 50  0001 C CNN
F 1 "GND" H 4255 2177 50  0000 C CNN
F 2 "" H 4250 2350 50  0001 C CNN
F 3 "" H 4250 2350 50  0001 C CNN
	1    4250 2350
	1    0    0    -1  
$EndComp
$Comp
L Device:C C11
U 1 1 61F89E26
P 4250 2200
F 0 "C11" H 4365 2246 50  0000 L CNN
F 1 "1 uF" H 4365 2155 50  0000 L CNN
F 2 "" H 4288 2050 50  0001 C CNN
F 3 "~" H 4250 2200 50  0001 C CNN
	1    4250 2200
	1    0    0    -1  
$EndComp
$Comp
L Device:R R5
U 1 1 61FAEC2D
P 3950 2200
F 0 "R5" H 4020 2246 50  0000 L CNN
F 1 "100K" V 3950 2100 50  0000 L CNN
F 2 "" V 3880 2200 50  0001 C CNN
F 3 "~" H 3950 2200 50  0001 C CNN
	1    3950 2200
	1    0    0    -1  
$EndComp
$Comp
L Switch:SW_DPST S?
U 1 1 61F6C28E
P 3600 1850
AR Path="/61F6C28E" Ref="S?"  Part="1" 
AR Path="/615D6456/61F6C28E" Ref="S2"  Part="1" 
F 0 "S2" H 3600 2175 50  0000 C CNN
F 1 "DPST-NO" H 3600 2084 50  0000 C CNN
F 2 "" H 3600 1850 50  0001 C CNN
F 3 "~" H 3600 1850 50  0001 C CNN
	1    3600 1850
	1    0    0    -1  
$EndComp
$Comp
L Z79Forth:74HCT14 U?
U 2 1 618554E9
P 4650 1950
AR Path="/618554E9" Ref="U?"  Part="2" 
AR Path="/615D6456/618554E9" Ref="U3"  Part="2" 
F 0 "U3" H 4650 2267 50  0000 C CNN
F 1 "74HC14" H 4650 2176 50  0000 C CNN
F 2 "" H 4650 1950 50  0001 C CNN
F 3 "http://www.ti.com/lit/gpn/sn74HC14" H 4650 1950 50  0001 C CNN
	2    4650 1950
	1    0    0    -1  
$EndComp
$Comp
L Device:R R4
U 1 1 61FADF14
P 3250 1500
F 0 "R4" H 3320 1546 50  0000 L CNN
F 1 "47K" V 3250 1450 50  0000 L CNN
F 2 "" V 3180 1500 50  0001 C CNN
F 3 "~" H 3250 1500 50  0001 C CNN
	1    3250 1500
	1    0    0    -1  
$EndComp
$Comp
L power:VCC #PWR0136
U 1 1 61F93E6B
P 3250 1350
F 0 "#PWR0136" H 3250 1200 50  0001 C CNN
F 1 "VCC" H 3265 1523 50  0000 C CNN
F 2 "" H 3250 1350 50  0001 C CNN
F 3 "" H 3250 1350 50  0001 C CNN
	1    3250 1350
	1    0    0    -1  
$EndComp
Wire Wire Line
	3250 1750 3250 1650
Wire Wire Line
	3250 1750 3400 1750
Wire Wire Line
	3400 1750 3400 1950
Connection ~ 3400 1750
Wire Wire Line
	3800 1950 3950 1950
Wire Wire Line
	3800 1750 3800 1950
Connection ~ 3800 1950
Wire Wire Line
	3950 2350 4250 2350
Connection ~ 4250 2350
Wire Wire Line
	3950 2050 3950 1950
Connection ~ 3950 1950
Wire Wire Line
	3950 1950 4250 1950
Wire Wire Line
	4250 2050 4250 1950
Connection ~ 4250 1950
Wire Wire Line
	4250 1950 4300 1950
Wire Wire Line
	1250 5750 1350 5750
Wire Wire Line
	1100 5850 1250 5850
Connection ~ 1250 5850
Wire Wire Line
	1250 5850 1350 5850
Wire Notes Line
	9800 6100 6300 6100
Wire Notes Line
	6300 6100 6300 2750
Wire Notes Line
	6300 2750 9800 2750
Wire Notes Line
	9800 2750 9800 6100
Text Notes 6450 3000 0    50   ~ 0
Experimental--not a part of the reference design
Text GLabel 10750 3800 2    36   Output ~ 0
~RD
$EndSCHEMATC
