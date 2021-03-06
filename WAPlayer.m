(* ::Package:: *)

BeginPackage["WAPlayer`"]

WAPlayer::usage=
"WAPlayer[address_String]:  Play a local or network audio file
WAPlayer[sound_Sound]:  Play a Sound object in Wolfram Language
WAPlayer[audio_Audio]:  Play an Audio object in Wolfram Language
WAPlayer[args___]:  It is equivalent to WAPlayer@Audio[args]"

InitializeSynthesizer::usage=
"InitializeSynthesizer[ ]:  Initialize all the instruments for the midi synthesizer"

LoadSoundBank::usage=
"LoadSoundBank[arg___]:  Load all the instruments found in FileNames[arg] for the midi synthesizer"

MMASoundFonts::usage=
"MMASoundFonts[ ]:  Find out the folder address where Mathematica stores its own sf2 sound banks."

Begin["`Private`"]

InitializeSynthesizer[]:=
(
	If[Quiet@Length@JLink`LoadedJavaObjects[]==0&&Quiet@Length@JLink`LoadedJavaClasses[]==0,
		JLink`ReinstallJava[JLink`JVMArguments->"-Xmx32g"]];
	
	JLink`LoadJavaClass["javax.sound.midi.MidiSystem"];
	
	If[JLink`JavaObjectQ[$Synthesizer],
		JLink`JavaBlock@Map[$Synthesizer@unloadInstrument[#]&,$Synthesizer@getLoadedInstruments[]];
		JLink`JavaBlock@$Synthesizer@loadAllInstruments@$Synthesizer@getDefaultSoundbank[],
		
		$Synthesizer=javax`sound`midi`MidiSystem`getSynthesizer[];
		$Synthesizer@open[]];
)

LoadSoundBank[arg___]:=
	JLink`JavaBlock@Map[$Synthesizer@loadAllInstruments@javax`sound`midi`MidiSystem`getSoundbank@JLink`JavaNew["java.io.File",#]&,FileNames[arg]]
	
MMASoundFonts[]:=
	Module[{paclets},
		PacletManager`Package`getPacletWithProgress[ "SoundFontIndex"];
		paclets = DeleteDuplicates@Values@Get@PacletManager`PacletResource["SoundFontIndex", "index.m"];
		PacletManager`Package`getPacletWithProgress/@paclets;
		PacletManager`Package`PgetLocation@First@PacletManager`PacletFind[#]&/@paclets
		]
	
Proxy:=
(
	JLink`LoadJavaClass["java.net.Proxy"];
	If["UseProxy"/.PacletManager`$InternetProxyRules,
		JLink`JavaNew[
			"java.net.Proxy",java`net`Proxy`NOUPROXY@type[]@valueOf["HTTP"],
			JLink`JavaNew["java.net.InetSocketAddress",Sequence@@("HTTP"/.PacletManager`$InternetProxyRules)]
			],
		java`net`Proxy`NOUPROXY]
)

MediaStream[bytes_List]:=
	JLink`JavaNew["java.io.ByteArrayInputStream",bytes]

MediaStream[url_String/;StringStartsQ[url,"http"|"https"~~"://"]]:=
	JLink`JavaNew["java.net.URL",url]@openConnection[Proxy]@getInputStream[]

MediaStream[address_String]:=
	JLink`JavaNew["java.io.FileInputStream",ExpandFileName[address]]


buttonstyle=
	<|
		"Play"->Graphics[{RGBColor[0,0.478,0.851],Triangle[{{-(Sqrt[2]/4),Sqrt[2]/2},{Sqrt[2]/4,0},{-(Sqrt[2]/4),-(Sqrt[2]/2)}}]},PlotRange->{{-1,1},{-1,1}},Background->None],
		"Pause"->Graphics[{RGBColor[0,0.478,0.851],Rectangle[{-(1/2),-(Sqrt[2]/2)},{-(1/6),Sqrt[2]/2}],Rectangle[{1/6,-(Sqrt[2]/2)},{1/2,Sqrt[2]/2}]},PlotRange->{{-1,1},{-1,1}},Background->None],
		"Stop"->Graphics[{RGBColor[0,0.478,0.851],Rectangle[{-(1/2),-(1/2)},{1/2,1/2}]},PlotRange->{{-1,1},{-1,1}},Background->None]
	|>;

buttonoption={Appearance->"Palette",FrameMargins->0,ImageSize->{25,25}};

MixPlayer[audio_,midi_]:=
	DynamicModule[{sequencer,options,stream,endposition},
	
		If[midi=!=None,
			If[!JLink`JavaObjectQ[$Synthesizer],
				InitializeSynthesizer[]];
			sequencer=javax`sound`midi`MidiSystem`getSequencer[False];
			sequencer@open[];
			JLink`JavaBlock@sequencer@getTransmitter[]@setReceiver@$Synthesizer@getReceiver[];
			JLink`JavaBlock@sequencer@setSequence@MediaStream[midi]];
	
		stream=
		If[audio=!=None,
			options=Options[audio];
			Audio`AudioStreamInternals`createAudioStream[
				audio,
				Lookup[options,AudioOutputDevice,Automatic]/.Automatic->$DefaultAudioOutputDevice,
				Lookup[options,SoundVolume,1],
				Round[0.2 Audio`Utilities`AudioSampleRate[audio]],
				3,
				4,
				"Internal",
				None,
				Lookup[options,AudioChannelAssignment,Automatic],
				False,
				False],
				
			Audio`AudioStreamInternals`createAudioStream[
				Audio[ConstantArray[0.,Ceiling[sequencer@getMicrosecondLength[]/10^4]],SampleRate->100],
				$DefaultAudioOutputDevice,
				1,
				20,
				3,
				4,
				"Internal",
				None,
				Automatic,
				False,
				False]
			];
			
		endposition=sequencer@getMicrosecondLength[]-1;

	Row[{
		Button[
			buttonstyle["Play"],
		
			If[sequencer@getMicrosecondPosition[]>=endposition,
				sequencer@setMicrosecondPosition[0]];
			sequencer@start[];
			Audio`AudioStreamInternals`playAudioStream@stream,
		
			buttonoption],
		
		Button[
			buttonstyle["Pause"],
			
			sequencer@stop[];
			Audio`AudioStreamInternals`pauseAudioStream@stream;
			sequencer@setMicrosecondPosition@Min[Round[10^6 QuantityMagnitude[stream["Position"],"Seconds"]],endposition],
			
			buttonoption],
			
		Button[
			buttonstyle["Stop"],
		
			sequencer@stop[];
			Audio`AudioStreamInternals`stopAudioStream@stream;
			sequencer@setMicrosecondPosition[0],
			
			buttonoption],
			
		Spacer[10],
		
		Slider[
			Dynamic[
				Min[QuantityMagnitude[stream["Position"],"Seconds"],stream["Duration"]],
				
					(sequencer@setMicrosecondPosition@Min[Round[10^6 #],endposition];
						If[!sequencer@isRunning[]&&stream["Status"]==="Playing",
							sequencer@start[]];
						stream["Position"]=Quantity[#,"Seconds"])&
				],
				
			{0,stream["Duration"]},ImageSize->{300,20},Appearance->"Labeled"]
			
		}],
		
	Deinitialization:>
		(Audio`AudioStreamInternals`removeAudioStream@stream;
			sequencer@close[];
			JLink`ReleaseJavaObject[sequencer])
		]


WAPlayer[address_String/;StringEndsQ[address,".mid"]]:=MixPlayer[None,address]

WAPlayer[midisequence_Sound`MIDISequence]:=MixPlayer[None,Sound`MIDISequenceToBytes@midisequence]

WAPlayer[sound_Sound]:=
	MixPlayer[
		If[#1===$Failed||#1===None,None,Audio[#1]],
		If[#2===$Failed||#2===None,None,Sound`MIDISequenceToBytes[#2]]
	]&@@Sound`NormalizeSound@sound

WAPlayer[args___]:=MixPlayer[Audio[args],None]

End[ ]

EndPackage[ ]
