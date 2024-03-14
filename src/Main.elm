module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewLink)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Tab as Tab
import Bootstrap.Table as Table
import Browser
import Browser.Navigation as Nav
import Debug
import Dialog
import Dict exposing (Dict)
import Html exposing (Html, a, br, button, div, fieldset, h1, h2, input, label, li, pre, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (checked, class, cols, href, placeholder, rows, selected, size, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, at)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as JE
import Json.Encode.Optional as Opt
import List.Extra exposing (getAt, removeAt, updateAt)
import List.Unique
import Maybe.Extra exposing (unwrap)
import Regex
import Reorderable as R
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , app : App
    , accessSpecNetwork : String
    , authorizedUser : String
    , customCategory : String
    , developer : String
    , editingExecDependsIndex : Maybe Int
    , editingInputSpecIndex : Maybe Int
    , editingOutputSpecIndex : Maybe Int
    , editingSysReqHost : Maybe SysReqHost
    , editingSysReqMinCores : Int
    , editingSysReqMinGpus : Int
    , editingSysReqMinMemory : Int
    , editingTimeoutPolicyIndex : Maybe Int
    , enableAccess : Bool
    , enableHttpsApp : Bool
    , enableSysReqClusterDefinition : Bool
    , error : Maybe String
    , execDependsStage : String
    , execDependsToModify : Maybe ExecDepends
    , httpsAppPort : Maybe Int
    , incomingJson : Maybe String
    , inputSpecChoice : String
    , inputSpecPattern : String
    , inputSpecToModify : Maybe InputSpec
    , inputSpecToModifyDefault : String
    , inputSpecToModifyError : Maybe String
    , jsonError : Maybe String
    , outputSpecPattern : String
    , outputSpecToModify : Maybe OutputSpec
    , outputSpecToModifyError : Maybe String
    , regionalOptionsResource : String
    , regionalOptionsToModify : Maybe ( String, RegionalOptions )
    , sysReqToModify : Maybe ( String, Maybe String, SystemRequirements )
    , tabState : Tab.State
    , timeoutPolicyToModify : Maybe ( String, TimeoutPolicy )
    }


type alias App =
    { name : String
    , title : String
    , summary : String
    , description : String
    , dxapi : String
    , version : String
    , developerNotes : String
    , categories : List String
    , developers : List String
    , authorizedUsers : List String
    , runSpec : RunSpec
    , inputSpec : List InputSpec
    , outputSpec : List OutputSpec
    , access : Maybe AccessSpec
    , httpsApp : Maybe HttpsApp
    , regionalOptions : Dict String RegionalOptions

    --, details : List String
    }


type alias AccessSpec =
    { network : List String
    , project : Maybe String
    , allProjects : Maybe String
    , developer : Maybe Bool
    , projectCreation : Maybe Bool
    }


type alias HttpsApp =
    { ports : List Int
    , sharedAccess : String
    }


type Resource
    = ResourceString String
    | ResourceList (List String)


type alias RegionalOptions =
    { resources : Resource
    , systemRequirements : Dict String SystemRequirements
    }


type RunSpecInterpreter
    = Bash
    | Python27
    | Python3


runSpecInterpreterToString : RunSpecInterpreter -> String
runSpecInterpreterToString interpreter =
    case interpreter of
        Bash ->
            "bash"

        Python27 ->
            "python2.7"

        Python3 ->
            "python3"


type alias RunSpec =
    { interpreter : RunSpecInterpreter
    , file : String
    , distribution : String
    , release : String
    , version : String
    , restartableEntryPoints : String
    , execDepends : List ExecDepends
    , timeoutPolicy : Dict String TimeoutPolicy
    , systemRequirements : Dict String SystemRequirements
    , assetDepends : List AssetDepends
    }


type AssetDepends
    = AssetDependsById { id : String }
    | AssetDependsByName { name : String }


type alias TimeoutPolicy =
    { hours : Int
    , minutes : Int
    }


type alias TimeoutPolicyTime =
    { hours : Int
    , minutes : Int
    }


type PackageManager
    = Apt
    | Cpan
    | Cran
    | Gem
    | Pip


packageManagerToString : PackageManager -> String
packageManagerToString packageManager =
    case packageManager of
        Apt ->
            "apt"

        Cpan ->
            "cpan"

        Cran ->
            "cran"

        Gem ->
            "gem"

        Pip ->
            "pip"


type alias ExecDepends =
    { name : String
    , packageManager : PackageManager
    , version : String
    , stages : List String
    }


type alias SystemRequirements =
    { instanceType : String
    , clusterSpec : Maybe ClusterSpec
    }


type alias ClusterSpec =
    { type_ : String
    , version : String
    , initialInstanceCount : Int
    , ports : String
    , bootstrapScript : String
    }


type alias OutputSpec =
    { name : String
    , label : String
    , help : String
    , optional : Bool
    , class : String
    , patterns : List String
    }



-- TODO: InputSpec.choices needs to list default value


type alias InputSpec =
    { name : String
    , label : String
    , class : InputSpecClass
    , optional : Bool
    , default : InputSpecDefaultValue
    , patterns : List String
    , choices : List String
    , type_ : Maybe String
    , help : String
    , group : String
    , suggestions : List InputSpecSuggestion
    }


type DefaultValueType
    = DefaultValueString
    | DefaultValueInt
    | DefaultValueFloat
    | DefaultValueBool


type InputSpecClass
    = InputSpecApplet
    | InputSpecArrayApplet
    | InputSpecArrayBoolean
    | InputSpecArrayFile
    | InputSpecArrayFloat
    | InputSpecArrayInt
    | InputSpecArrayRecord
    | InputSpecArrayString
    | InputSpecBool
    | InputSpecFile
    | InputSpecFloat
    | InputSpecHash
    | InputSpecInt
    | InputSpecRecord
    | InputSpecString


type alias InputSpecSuggestion =
    { name : String
    , project : String
    , path : String
    , region : String
    }


inputSpecClassToString : InputSpecClass -> String
inputSpecClassToString class =
    case class of
        InputSpecApplet ->
            "applet"

        InputSpecArrayApplet ->
            "array:applet"

        InputSpecArrayBoolean ->
            "array:boolean"

        InputSpecArrayFile ->
            "array:file"

        InputSpecArrayFloat ->
            "array:float"

        InputSpecArrayInt ->
            "array:int"

        InputSpecArrayRecord ->
            "array:record"

        InputSpecArrayString ->
            "array:string"

        InputSpecBool ->
            "boolean"

        InputSpecFile ->
            "file"

        InputSpecFloat ->
            "float"

        InputSpecHash ->
            "hash"

        InputSpecInt ->
            "int"

        InputSpecRecord ->
            "record"

        InputSpecString ->
            "string"


type InputSpecDefaultValue
    = DefaultValString String
    | DefaultValInt Int
    | DefaultValFloat Float
    | DefaultValBool Bool


defaultValueToString : InputSpecDefaultValue -> String
defaultValueToString val =
    case val of
        DefaultValInt i ->
            String.fromInt i

        DefaultValFloat f ->
            String.fromFloat f

        DefaultValBool b ->
            if b then
                "True"

            else
                "False"

        DefaultValString v ->
            v


commonInputSpecPatterns : List String
commonInputSpecPatterns =
    [ "*"
    , "*.bai"
    , "*.bam"
    , "*.fa"
    , "*.faa"
    , "*.fasta"
    , "*.fastq"
    , "*.fna"
    , "*.fq"
    , "*.sam"
    ]


validRegions : List String
validRegions =
    [ "*"
    , "aws:us-east-1"
    , "aws:eu-central-1"
    , "aws:ap-southeast-2"
    , "aws:eu-west-2"
    , "aws:eu-west-2-g"
    , "azure:westus"
    , "azure:westeurope"
    ]


validClusterSpecType : List String
validClusterSpecType =
    [ "", "generic", "dxspark", "apachespark" ]


validClusterSpecVersions : List String
validClusterSpecVersions =
    [ "", "2.4.4", "3.2.0" ]


validAccessSpecOptions : List String
validAccessSpecOptions =
    [ "", "VIEW", "UPLOAD", "CONTRIBUTE", "ADMINISTER" ]


validCategories : List String
validCategories =
    [ "Annotation"
    , "Assembly"
    , "Debugging"
    , "Export"
    , "Import"
    , "Mappings Manipulation"
    , "Read Manipulation"
    , "Read Mapping"
    , "Reports"
    , "RNA-Seq"
    , "Statistics"
    , "Structural Variation"
    , "Variation Calling"
    ]


type SysReqHost
    = Aws
    | Azure


sysReqHostToString : SysReqHost -> String
sysReqHostToString host =
    case host of
        Azure ->
            "Azure"

        Aws ->
            "AWS"


type alias InstanceType =
    { host : SysReqHost
    , name : String
    , cores : Int
    , memory : Float
    , storage : Int
    , gpus : Int
    }


validInstanceTypes : List InstanceType
validInstanceTypes =
    [ { host = Aws
      , name = "mem1_ssd1_x2"
      , cores = 2
      , memory = 3.8
      , storage = 40
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_x4"
      , cores = 4
      , memory = 7.5
      , storage = 80
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_x8"
      , cores = 8
      , memory = 15.0
      , storage = 160
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_x16"
      , cores = 16
      , memory = 30.0
      , storage = 320
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_x32"
      , cores = 32
      , memory = 60.0
      , storage = 640
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_x36"
      , cores = 36
      , memory = 72.0
      , storage = 900
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_v2_x2"
      , cores = 2
      , memory = 4.0
      , storage = 50
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_v2_x4"
      , cores = 4
      , memory = 8.0
      , storage = 100
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_v2_x8"
      , cores = 8
      , memory = 16.0
      , storage = 200
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_v2_x16"
      , cores = 16
      , memory = 32.0
      , storage = 400
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_v2_x36"
      , cores = 36
      , memory = 72.0
      , storage = 900
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_v2_x72"
      , cores = 72
      , memory = 144.0
      , storage = 1800
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_x2"
      , cores = 2
      , memory = 3.8
      , storage = 160
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_x4"
      , cores = 4
      , memory = 7.5
      , storage = 320
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_x8"
      , cores = 8
      , memory = 15.0
      , storage = 640
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_x16"
      , cores = 16
      , memory = 30.0
      , storage = 1280
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_x36"
      , cores = 36
      , memory = 60.0
      , storage = 2880
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_v2_x2"
      , cores = 2
      , memory = 4.0
      , storage = 160
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_v2_x4"
      , cores = 4
      , memory = 8.0
      , storage = 320
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_v2_x8"
      , cores = 8
      , memory = 16.0
      , storage = 640
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_v2_x16"
      , cores = 16
      , memory = 32.0
      , storage = 1280
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_v2_x36"
      , cores = 36
      , memory = 72.0
      , storage = 2880
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd2_v2_x72"
      , cores = 72
      , memory = 144.0
      , storage = 5760
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_hdd2_x8"
      , cores = 8
      , memory = 7.0
      , storage = 1680
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_hdd2_x32"
      , cores = 32
      , memory = 60.5
      , storage = 3360
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_v2_x16"
      , cores = 16
      , memory = 128.0
      , storage = 600
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_v2_x32"
      , cores = 32
      , memory = 256.0
      , storage = 1200
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_v2_x48"
      , cores = 48
      , memory = 384.0
      , storage = 1800
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_v2_x64"
      , cores = 64
      , memory = 512.0
      , storage = 3200
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_v2_x96"
      , cores = 96
      , memory = 768.0
      , storage = 3600
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd2_x4"
      , cores = 4
      , memory = 30.5
      , storage = 800
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd2_x8"
      , cores = 8
      , memory = 61.0
      , storage = 1600
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd2_x16"
      , cores = 16
      , memory = 122.0
      , storage = 3200
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd2_x32"
      , cores = 32
      , memory = 244.0
      , storage = 6400
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd2_v2_x2"
      , cores = 2
      , memory = 15.25
      , storage = 475
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd2_v2_x4"
      , cores = 4
      , memory = 30.5
      , storage = 950
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd2_v2_x8"
      , cores = 8
      , memory = 61.0
      , storage = 1900
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd2_v2_x16"
      , cores = 16
      , memory = 122.0
      , storage = 3800
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd2_v2_x32"
      , cores = 32
      , memory = 244.0
      , storage = 7600
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd2_v2_x64"
      , cores = 64
      , memory = 488.0
      , storage = 15200
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd3_x2"
      , cores = 2
      , memory = 16.0
      , storage = 1250
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd3_x4"
      , cores = 4
      , memory = 32.0
      , storage = 2500
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd3_x8"
      , cores = 8
      , memory = 64.0
      , storage = 5000
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd3_x12"
      , cores = 12
      , memory = 96.0
      , storage = 7500
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd3_x24"
      , cores = 24
      , memory = 192.0
      , storage = 15000
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd3_x48"
      , cores = 48
      , memory = 384.0
      , storage = 30000
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd3_x96"
      , cores = 96
      , memory = 768.0
      , storage = 60000
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_hdd2_x2"
      , cores = 2
      , memory = 17.1
      , storage = 420
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_hdd2_x4"
      , cores = 4
      , memory = 34.2
      , storage = 850
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_hdd2_x8"
      , cores = 8
      , memory = 68.4
      , storage = 1680
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_hdd2_v2_x2"
      , cores = 2
      , memory = 16.0
      , storage = 500
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_hdd2_v2_x4"
      , cores = 4
      , memory = 32.0
      , storage = 1000
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_hdd2_v2_x8"
      , cores = 8
      , memory = 64.0
      , storage = 2000
      , gpus = 0
      }
    , { host = Aws
      , name = ""
      , cores = 0
      , memory = 0.0
      , storage = 0
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem1_ssd1_x4"
      , cores = 4
      , memory = 7.8
      , storage = 64
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem1_ssd1_x8"
      , cores = 8
      , memory = 15.7
      , storage = 128
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem1_ssd1_x16"
      , cores = 16
      , memory = 31.4
      , storage = 254
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem2_ssd1_x1"
      , cores = 1
      , memory = 3.5
      , storage = 128
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem2_ssd1_x2"
      , cores = 2
      , memory = 7.0
      , storage = 128
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem2_ssd1_x4"
      , cores = 4
      , memory = 14.0
      , storage = 128
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem2_ssd1_x8"
      , cores = 8
      , memory = 28.0
      , storage = 256
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem2_ssd1_x16"
      , cores = 16
      , memory = 56.0
      , storage = 512
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem3_ssd1_x2"
      , cores = 2
      , memory = 14.0
      , storage = 128
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem3_ssd1_x4"
      , cores = 4
      , memory = 28.0
      , storage = 128
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem3_ssd1_x8"
      , cores = 8
      , memory = 56.0
      , storage = 256
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem3_ssd1_x16"
      , cores = 16
      , memory = 112.0
      , storage = 512
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem3_ssd1_x20"
      , cores = 20
      , memory = 140.0
      , storage = 640
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem4_ssd1_x2"
      , cores = 2
      , memory = 28.0
      , storage = 128
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem4_ssd1_x4"
      , cores = 4
      , memory = 56.0
      , storage = 128
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem4_ssd1_x8"
      , cores = 8
      , memory = 112.0
      , storage = 256
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem4_ssd1_x16"
      , cores = 16
      , memory = 224.0
      , storage = 512
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem4_ssd1_x32"
      , cores = 32
      , memory = 448.0
      , storage = 1024
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem5_ssd2_x64*"
      , cores = 64
      , memory = 1792.0
      , storage = 8192
      , gpus = 0
      }
    , { host = Azure
      , name = "azure:mem5_ssd2_x128*"
      , cores = 128
      , memory = 3892.0
      , storage = 16384
      , gpus = 0
      }
    , { host = Aws
      , name = "mem1_ssd1_gpu2_x8"
      , cores = 8
      , memory = 15.0
      , storage = 60
      , gpus = 2
      }
    , { host = Aws
      , name = "mem1_ssd1_gpu2_x32"
      , cores = 32
      , memory = 60.0
      , storage = 240
      , gpus = 4
      }
    , { host = Aws
      , name = "mem2_ssd1_gpu_x16"
      , cores = 16
      , memory = 64.0
      , storage = 225
      , gpus = 1
      }
    , { host = Aws
      , name = "mem2_ssd1_gpu_x32"
      , cores = 32
      , memory = 128.0
      , storage = 900
      , gpus = 1
      }
    , { host = Aws
      , name = "mem2_ssd1_gpu_x48"
      , cores = 48
      , memory = 192.0
      , storage = 900
      , gpus = 4
      }
    , { host = Aws
      , name = "mem2_ssd1_gpu_x64"
      , cores = 64
      , memory = 256.0
      , storage = 900
      , gpus = 1
      }
    , { host = Aws
      , name = "mem3_ssd1_gpu_x8"
      , cores = 8
      , memory = 61.0
      , storage = 160
      , gpus = 1
      }
    , { host = Aws
      , name = "mem3_ssd1_gpu_x32"
      , cores = 32
      , memory = 244.0
      , storage = 640
      , gpus = 4
      }
    , { host = Aws
      , name = "mem3_ssd1_gpu_x64"
      , cores = 64
      , memory = 488.0
      , storage = 1280
      , gpus = 8
      }
    , { host = Azure
      , name = "azure:mem3_ssd2_gpu4_x64"
      , cores = 64
      , memory = 488
      , storage = 2048
      , gpus = 4
      }
    , { host = Aws
      , name = "mem2_ssd1_x2"
      , cores = 2
      , memory = 7.5
      , storage = 40
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_ssd1_x4"
      , cores = 4
      , memory = 15.0
      , storage = 80
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_ssd1_x8"
      , cores = 8
      , memory = 30.0
      , storage = 160
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_ssd1_v2_x2"
      , cores = 2
      , memory = 8.0
      , storage = 75
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_ssd1_v2_x4"
      , cores = 4
      , memory = 16.0
      , storage = 150
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_ssd1_v2_x8"
      , cores = 8
      , memory = 32.0
      , storage = 300
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_ssd1_v2_x16"
      , cores = 16
      , memory = 64.0
      , storage = 600
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_ssd1_v2_x32"
      , cores = 32
      , memory = 128.0
      , storage = 1200
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_ssd1_v2_x48"
      , cores = 48
      , memory = 144.0
      , storage = 1800
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_ssd1_v2_x64"
      , cores = 64
      , memory = 256.0
      , storage = 2400
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_ssd1_v2_x96"
      , cores = 96
      , memory = 384.0
      , storage = 3600
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_hdd2_x1"
      , cores = 1
      , memory = 3.8
      , storage = 410
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_hdd2_x2"
      , cores = 2
      , memory = 7.5
      , storage = 840
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_hdd2_x4"
      , cores = 4
      , memory = 15.0
      , storage = 1680
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_hdd2_v2_x2"
      , cores = 2
      , memory = 8.0
      , storage = 1000
      , gpus = 0
      }
    , { host = Aws
      , name = "mem2_hdd2_v2_x4"
      , cores = 4
      , memory = 16.0
      , storage = 2000
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_x2"
      , cores = 2
      , memory = 15.0
      , storage = 40
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_x4"
      , cores = 4
      , memory = 30.5
      , storage = 80
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_x8"
      , cores = 8
      , memory = 61.0
      , storage = 160
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_x16"
      , cores = 16
      , memory = 122.0
      , storage = 320
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_x32"
      , cores = 32
      , memory = 244.0
      , storage = 640
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_v2_x2"
      , cores = 2
      , memory = 16.0
      , storage = 75
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_v2_x4"
      , cores = 4
      , memory = 32.0
      , storage = 150
      , gpus = 0
      }
    , { host = Aws
      , name = "mem3_ssd1_v2_x8"
      , cores = 8
      , memory = 64.0
      , storage = 300
      , gpus = 0
      }
    , { host = Aws
      , name = "mem4_ssd1_x128"
      , cores = 128
      , memory = 1952.0
      , storage = 3840
      , gpus = 0
      }
    ]


type Direction
    = Up
    | Down


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , app = initialApp
      , accessSpecNetwork = ""
      , authorizedUser = ""
      , customCategory = ""
      , developer = ""
      , editingExecDependsIndex = Nothing
      , editingInputSpecIndex = Nothing
      , editingOutputSpecIndex = Nothing
      , editingSysReqHost = Nothing
      , editingSysReqMinCores = 0
      , editingSysReqMinGpus = 0
      , editingSysReqMinMemory = 0
      , editingTimeoutPolicyIndex = Nothing
      , enableAccess = False
      , enableHttpsApp = False
      , enableSysReqClusterDefinition = False
      , error = Nothing
      , execDependsStage = ""
      , execDependsToModify = Nothing
      , httpsAppPort = Nothing
      , incomingJson = Nothing
      , inputSpecChoice = ""
      , inputSpecPattern = ""
      , inputSpecToModify = Nothing
      , inputSpecToModifyDefault = ""
      , inputSpecToModifyError = Nothing
      , jsonError = Nothing
      , outputSpecPattern = ""
      , outputSpecToModify = Nothing
      , outputSpecToModifyError = Nothing
      , regionalOptionsResource = ""
      , regionalOptionsToModify = Nothing
      , sysReqToModify = Nothing
      , tabState = Tab.initialState
      , timeoutPolicyToModify = Nothing
      }
    , Cmd.none
    )


initialApp =
    { name = "my_new_app"
    , title = "My New App"
    , summary = "App Summary"
    , description = ""
    , dxapi = "0.0.1"
    , version = "0.0.1"
    , developerNotes = ""
    , categories = []
    , developers = []
    , authorizedUsers = []
    , runSpec = initialRunSpec
    , inputSpec = []
    , outputSpec = []
    , access = Nothing
    , httpsApp = Nothing
    , regionalOptions = Dict.empty

    --, details = []
    }


initialAccessSpec =
    { network = []
    , project = Nothing
    , allProjects = Nothing
    , developer = Nothing
    , projectCreation = Nothing
    }


validHttpsPorts : List Int
validHttpsPorts =
    [ 443, 8080, 8081 ]


initialHttpsApp =
    { ports = validHttpsPorts
    , sharedAccess = "NONE"
    }


initialRegionalOptions =
    { resources = ResourceList []
    , systemRequirements = Dict.empty
    }


initialRunSpec =
    { interpreter = Bash
    , file = "src/script.sh"
    , distribution = "Ubuntu"
    , release = "20.04"
    , version = "0"
    , restartableEntryPoints = ""
    , execDepends = []
    , timeoutPolicy = Dict.empty
    , systemRequirements = Dict.empty
    , assetDepends = []
    }


initialSystemRequirements =
    { instanceType = "mem2_hdd2_x2"
    , clusterSpec = Nothing
    }


initialTimeoutPolicy =
    { hours = 48
    , minutes = 0
    }


initialClusterSpec =
    { type_ = "generic"
    , version = ""
    , initialInstanceCount = 1
    , ports = ""
    , bootstrapScript = ""
    }


initialExecDepends =
    { name = "package_name"
    , packageManager = Apt
    , version = ""
    , stages = []
    }


initialOutputSpec =
    { name = "output_name"
    , class = "file"
    , label = "Label"
    , help = ""
    , optional = False
    , patterns = []
    }


initialInputSpec =
    { name = "input_name"
    , label = "Human-readable label"
    , class = InputSpecString
    , optional = False
    , default = DefaultValString ""
    , patterns = []
    , choices = []
    , type_ = Nothing
    , help = ""
    , group = ""
    , suggestions = []
    }



-- UPDATE


type Msg
    = CloseExecDependsDialog
    | CloseInputSpecDialog
    | CloseJsonDialog
    | CloseOutputSpecDialog
    | CloseRegionalOptionsDialog
    | CloseSysReqDialog
    | CloseTimeoutPolicyDialog
    | DecodeIncomingJson
    | DeleteAccessSpecNetwork String
    | DeleteAppCategory String
    | DeleteAuthorizedUser String
    | DeleteDeveloper String
    | DeleteExecDepends Int
    | DeleteExecDependsStage String
    | DeleteHttpsAppPort Int
    | DeleteInputSpec Int
    | DeleteInputSpecChoice String
    | DeleteInputSpecPattern String
    | DeleteOutputSpec Int
    | DeleteOutputSpecPattern String
    | DeleteRegionalOptions String
    | DeleteRegionalOptionsResource String
    | DeleteSysReq Int
    | DeleteTimeoutPolicy Int
    | EnableAccess Bool
    | EnableHttpsApp Bool
    | LinkClicked Browser.UrlRequest
    | ModifyExecDependsDialog ExecDepends (Maybe Int)
    | ModifyInputSpecDialog InputSpec (Maybe Int)
    | ModifyOutputSpecDialog OutputSpec (Maybe Int)
    | ModifyRegionalOptionsDialog String RegionalOptions (Maybe Int)
    | ModifySysReqDialog String (Maybe String) SystemRequirements
    | ModifyTimeoutPolicyDialog String TimeoutPolicy (Maybe Int)
    | SaveExecDepends
    | SaveInputSpec
    | SaveInputSpecDefault
    | SaveOutputSpec
    | SaveRegionalOptions
    | SaveSysReqToRegionalOptions String
    | SaveSysReqToRunSpec
    | SaveTimeoutPolicy
    | SetExecDependsToModify Int
    | SetInputSpecToModify Int
    | SetOutputSpecToModify Int
    | SetRegionalOptionsSysReqToModify String String SystemRequirements
    | SetRegionalOptionsToModify String
    | SetRunSpecSysReqToModify String
    | SetTimeoutPolicyToModify Int
    | ShowJsonDialog
    | TabMsg Tab.State
    | UpdateAccessSpecAddNetwork
    | UpdateAccessSpecAllProjects String
    | UpdateAccessSpecDeveloper Bool
    | UpdateAccessSpecNetwork String
    | UpdateAccessSpecProject String
    | UpdateAccessSpecProjectCreation Bool
    | UpdateAddAuthorizedUser
    | UpdateAddDeveloper
    | UpdateAppAddCustomCategory
    | UpdateAppCategories String
    | UpdateAppDescription String
    | UpdateAppDeveloperNotes String
    | UpdateAppDxapi String
    | UpdateAppName String
    | UpdateAppSummary String
    | UpdateAppTitle String
    | UpdateAppVersion String
    | UpdateAuthorizedUser String
    | UpdateCustomCategory String
    | UpdateDeveloper String
    | UpdateExecDependsAddStage
    | UpdateExecDependsName String
    | UpdateExecDependsPackageManager String
    | UpdateExecDependsStage String
    | UpdateExecDependsVersion String
    | UpdateHttpsAppAddPort String
    | UpdateHttpsAppSharedAccess String
    | UpdateIncomingJson String
    | UpdateInputSpecAddChoice
    | UpdateInputSpecAddCustomPattern
    | UpdateInputSpecAddPattern String
    | UpdateInputSpecChoice String
    | UpdateInputSpecClass String
    | UpdateInputSpecCustomPattern String
    | UpdateInputSpecDefault String
    | UpdateInputSpecHelp String
    | UpdateInputSpecLabel String
    | UpdateInputSpecName String
    | UpdateInputSpecOptional Bool
    | UpdateOutputSpecAddCustomPattern
    | UpdateOutputSpecAddPattern String
    | UpdateOutputSpecClass String
    | UpdateOutputSpecCustomPattern String
    | UpdateOutputSpecName String
    | UpdateRegionalOptionsAddResource
    | UpdateRegionalOptionsRegionName String
    | UpdateRegionalOptionsResource String
    | UpdateRunSpecDistribution String
    | UpdateRunSpecFile String
    | UpdateRunSpecInterpreter String
    | UpdateRunSpecRelease String
    | UpdateRunSpecRestartableEntryPoints String
    | UpdateRunSpecVersion String
    | UpdateSysReqClusterSpecBootstrapScript String
    | UpdateSysReqClusterSpecInitialInstanceCount String
    | UpdateSysReqClusterSpecPorts String
    | UpdateSysReqClusterSpecType String
    | UpdateSysReqClusterSpecVersion String
    | UpdateSysReqEnableCluster Bool
    | UpdateSysReqEntryPointName String
    | UpdateSysReqMinGpus String
    | UpdateSysReqMinCores String
    | UpdateSysReqMinMemory String
    | UpdateSysReqHost String
    | UpdateSysReqInstanceType String
    | UpdateTimeoutPolicyEntryPointName String
    | UpdateTimeoutPolicyHours String
    | UpdateTimeoutPolicyMinutes String
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseExecDependsDialog ->
            ( { model
                | execDependsToModify = Nothing
                , editingExecDependsIndex = Nothing
              }
            , Cmd.none
            )

        CloseInputSpecDialog ->
            ( { model
                | inputSpecToModify = Nothing
                , editingInputSpecIndex = Nothing
              }
            , Cmd.none
            )

        CloseOutputSpecDialog ->
            ( { model
                | outputSpecToModify = Nothing
                , editingOutputSpecIndex = Nothing
              }
            , Cmd.none
            )

        CloseRegionalOptionsDialog ->
            ( { model
                | regionalOptionsToModify = Nothing
              }
            , Cmd.none
            )

        CloseSysReqDialog ->
            ( { model
                | sysReqToModify = Nothing
                , editingSysReqHost = Nothing
              }
            , Cmd.none
            )

        CloseTimeoutPolicyDialog ->
            ( { model
                | timeoutPolicyToModify = Nothing
                , editingTimeoutPolicyIndex = Nothing
              }
            , Cmd.none
            )

        CloseJsonDialog ->
            ( { model | incomingJson = Nothing }, Cmd.none )

        DeleteAccessSpecNetwork network ->
            let
                app =
                    model.app

                newAccess =
                    Maybe.map
                        (\access ->
                            { access
                                | network =
                                    List.filter (\n -> n /= network) access.network
                            }
                        )
                        app.access

                newApp =
                    { app | access = newAccess }
            in
            ( { model | app = newApp }, Cmd.none )

        DeleteAppCategory cat ->
            let
                app =
                    model.app

                newApp =
                    { app
                        | categories =
                            List.filter (\c -> c /= cat) app.categories
                    }
            in
            ( { model | app = newApp }, Cmd.none )

        DeleteAuthorizedUser val ->
            let
                app =
                    model.app

                newApp =
                    { app
                        | authorizedUsers =
                            List.filter (\u -> u /= val) app.authorizedUsers
                    }
            in
            ( { model | app = newApp }, Cmd.none )

        DeleteDeveloper val ->
            let
                app =
                    model.app

                newApp =
                    { app
                        | developers =
                            List.filter (\d -> d /= val) app.developers
                    }
            in
            ( { model | app = newApp }, Cmd.none )

        DeleteExecDepends index ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                newRunSpec =
                    { runSpec
                        | execDepends = removeAt index runSpec.execDepends
                    }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        DeleteExecDependsStage stage ->
            let
                newExec =
                    Maybe.map
                        (\exec ->
                            { exec
                                | stages =
                                    List.filter
                                        (\s -> s /= stage)
                                        exec.stages
                            }
                        )
                        model.execDependsToModify
            in
            ( { model | execDependsToModify = newExec }, Cmd.none )

        DeleteHttpsAppPort index ->
            let
                app =
                    model.app

                newHttpsApp =
                    Maybe.map
                        (\httpsApp ->
                            { httpsApp
                                | ports = removeAt index httpsApp.ports
                            }
                        )
                        app.httpsApp

                newApp =
                    { app | httpsApp = newHttpsApp }
            in
            ( { model | app = newApp }, Cmd.none )

        DeleteInputSpec index ->
            let
                app =
                    model.app

                newApp =
                    { app | inputSpec = removeAt index app.inputSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        DeleteInputSpecChoice choice ->
            let
                newSpec =
                    Maybe.map
                        (\spec ->
                            { spec
                                | choices =
                                    List.filter (\c -> c /= choice)
                                        spec.choices
                            }
                        )
                        model.inputSpecToModify
            in
            ( { model | inputSpecToModify = newSpec }, Cmd.none )

        DeleteInputSpecPattern pattern ->
            let
                newSpec =
                    Maybe.map
                        (\spec ->
                            { spec
                                | patterns =
                                    List.filter (\p -> p /= pattern)
                                        spec.patterns
                            }
                        )
                        model.inputSpecToModify
            in
            ( { model | inputSpecToModify = newSpec }, Cmd.none )

        DecodeIncomingJson ->
            let
                newModel =
                    decodeIncomingJson model

                newJson =
                    case newModel.jsonError of
                        Nothing ->
                            Nothing

                        _ ->
                            model.incomingJson
            in
            ( { newModel | incomingJson = newJson }, Cmd.none )

        DeleteOutputSpec index ->
            let
                app =
                    model.app

                newApp =
                    { app | outputSpec = removeAt index app.outputSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        DeleteOutputSpecPattern pattern ->
            let
                newSpec =
                    Maybe.map
                        (\spec ->
                            { spec
                                | patterns =
                                    List.filter (\p -> p /= pattern)
                                        spec.patterns
                            }
                        )
                        model.outputSpecToModify
            in
            ( { model | outputSpecToModify = newSpec }, Cmd.none )

        DeleteRegionalOptionsResource resource ->
            let
                newOpts =
                    Maybe.map
                        (\( regionName, opts ) ->
                            let
                                curResources =
                                    case opts.resources of
                                        ResourceString s ->
                                            [ s ]

                                        ResourceList vals ->
                                            vals

                                newResources =
                                    List.filter
                                        (\r -> r /= resource)
                                        curResources
                            in
                            ( regionName
                            , { opts | resources = ResourceList newResources }
                            )
                        )
                        model.regionalOptionsToModify
            in
            ( { model | regionalOptionsToModify = newOpts }, Cmd.none )

        DeleteRegionalOptions regionName ->
            let
                app =
                    model.app

                newOpts =
                    Dict.toList app.regionalOptions
                        |> List.filter
                            (\( regName, opt ) -> regName /= regionName)
                        |> Dict.fromList

                newApp =
                    { app | regionalOptions = newOpts }
            in
            ( { model | app = newApp }, Cmd.none )

        DeleteSysReq index ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                newSysReqs =
                    Dict.fromList
                        (removeAt index
                            (Dict.toList runSpec.systemRequirements)
                        )

                newRunSpec =
                    { runSpec | systemRequirements = newSysReqs }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        DeleteTimeoutPolicy index ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                newTimeoutPolicy =
                    Dict.fromList
                        (removeAt index
                            (Dict.toList runSpec.timeoutPolicy)
                        )

                newRunSpec =
                    { runSpec | timeoutPolicy = newTimeoutPolicy }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        EnableAccess enabled ->
            let
                app =
                    model.app

                newAccessSpec =
                    if enabled then
                        Just initialAccessSpec

                    else
                        Nothing

                newApp =
                    { app | access = newAccessSpec }
            in
            ( { model
                | enableAccess = enabled
                , app = newApp
              }
            , Cmd.none
            )

        EnableHttpsApp enabled ->
            let
                app =
                    model.app

                newHttpsApp =
                    if enabled then
                        Just initialHttpsApp

                    else
                        Nothing

                newApp =
                    { app | httpsApp = newHttpsApp }
            in
            ( { model
                | enableHttpsApp = enabled
                , app = newApp
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ModifyExecDependsDialog newExecDepends index ->
            ( { model
                | execDependsToModify = Just newExecDepends
                , editingExecDependsIndex = index
              }
            , Cmd.none
            )

        ModifyInputSpecDialog newInputSpec index ->
            ( { model
                | inputSpecToModify = Just newInputSpec
                , editingInputSpecIndex = index
              }
            , Cmd.none
            )

        ModifyOutputSpecDialog newOutputSpec index ->
            ( { model
                | outputSpecToModify = Just newOutputSpec
                , editingOutputSpecIndex = index
              }
            , Cmd.none
            )

        ModifyRegionalOptionsDialog regionName newOpts index ->
            ( { model
                | regionalOptionsToModify = Just ( regionName, newOpts )
              }
            , Cmd.none
            )

        ModifySysReqDialog entryPointName regionName newSysReq ->
            -- This allows use by both RunSpec and RegionalOptions
            let
                instanceType =
                    newSysReq.instanceType

                newHost =
                    if String.contains "azure" instanceType then
                        Azure

                    else
                        Aws
            in
            ( { model
                | sysReqToModify =
                    Just ( entryPointName, regionName, newSysReq )
                , editingSysReqHost = Just newHost
              }
            , Cmd.none
            )

        ModifyTimeoutPolicyDialog entryPointName newTimeoutPolicy index ->
            ( { model
                | timeoutPolicyToModify =
                    Just ( entryPointName, newTimeoutPolicy )
                , editingTimeoutPolicyIndex = index
              }
            , Cmd.none
            )

        SaveExecDepends ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                newExecDepends =
                    case model.execDependsToModify of
                        Just val ->
                            case model.editingExecDependsIndex of
                                Just i ->
                                    updateAt i
                                        (\_ -> val)
                                        runSpec.execDepends

                                _ ->
                                    runSpec.execDepends ++ [ val ]

                        _ ->
                            runSpec.execDepends

                newRunSpec =
                    { runSpec | execDepends = newExecDepends }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model
                | app = newApp
                , execDependsToModify = Nothing
                , editingExecDependsIndex = Nothing
              }
            , Cmd.none
            )

        SaveInputSpecDefault ->
            let
                app =
                    model.app

                toBool s =
                    String.toLower s == "true"

                curDefault =
                    model.inputSpecToModifyDefault

                convertedDefault =
                    case model.inputSpecToModify of
                        Just spec ->
                            case spec.class of
                                InputSpecBool ->
                                    Just (DefaultValBool (toBool curDefault))

                                InputSpecInt ->
                                    Maybe.map
                                        (\i -> DefaultValInt i)
                                        (String.toInt curDefault)

                                InputSpecFloat ->
                                    Maybe.map
                                        (\f -> DefaultValFloat f)
                                        (String.toFloat curDefault)

                                _ ->
                                    Just (DefaultValString curDefault)

                        _ ->
                            Nothing

                ( newInputSpec, newErr ) =
                    case model.inputSpecToModify of
                        Just inputSpecToModify ->
                            case convertedDefault of
                                Just def ->
                                    ( Just { inputSpecToModify | default = def }
                                    , Nothing
                                    )

                                _ ->
                                    ( Just inputSpecToModify
                                    , Just "Invalid default"
                                    )

                        _ ->
                            ( Nothing, Nothing )
            in
            ( { model
                | inputSpecToModify = newInputSpec
                , inputSpecToModifyError = newErr
              }
            , Cmd.none
            )

        SaveInputSpec ->
            let
                app =
                    model.app

                newInputSpec =
                    case model.inputSpecToModify of
                        Just spec ->
                            case model.editingInputSpecIndex of
                                Just i ->
                                    updateAt i (\_ -> spec) app.inputSpec

                                _ ->
                                    app.inputSpec ++ [ spec ]

                        _ ->
                            app.inputSpec

                newApp =
                    { app | inputSpec = newInputSpec }
            in
            ( { model
                | app = newApp
                , inputSpecToModify = Nothing
                , inputSpecToModifyDefault = ""
                , inputSpecToModifyError = Nothing
                , editingInputSpecIndex = Nothing
              }
            , Cmd.none
            )

        SaveOutputSpec ->
            let
                app =
                    model.app

                newOutputSpec =
                    case model.outputSpecToModify of
                        Just spec ->
                            case model.editingOutputSpecIndex of
                                Just i ->
                                    updateAt i (\_ -> spec) app.outputSpec

                                _ ->
                                    app.outputSpec ++ [ spec ]

                        _ ->
                            app.outputSpec

                newApp =
                    { app | outputSpec = newOutputSpec }
            in
            ( { model
                | app = newApp
                , outputSpecToModify = Nothing
                , outputSpecToModifyError = Nothing
                , editingOutputSpecIndex = Nothing
              }
            , Cmd.none
            )

        SaveRegionalOptions ->
            let
                app =
                    model.app

                curOpts =
                    app.regionalOptions

                ( newOpts, editingOpt ) =
                    case model.regionalOptionsToModify of
                        Just ( regionName, opt ) ->
                            ( Dict.update
                                regionName
                                (\_ -> Just opt)
                                curOpts
                            , Nothing
                            )

                        _ ->
                            ( curOpts, model.regionalOptionsToModify )

                newApp =
                    { app | regionalOptions = newOpts }
            in
            ( { model
                | app = newApp
                , regionalOptionsToModify = editingOpt
              }
            , Cmd.none
            )

        SaveSysReqToRunSpec ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                sysReqs =
                    runSpec.systemRequirements

                ( newSysReq, newEditingSysReq ) =
                    case model.sysReqToModify of
                        Just ( entryPointName, _, req ) ->
                            ( Dict.update
                                entryPointName
                                (\_ -> Just req)
                                sysReqs
                            , Nothing
                            )

                        _ ->
                            ( sysReqs, model.sysReqToModify )

                newRunSpec =
                    { runSpec | systemRequirements = newSysReq }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model
                | app = newApp
                , sysReqToModify = newEditingSysReq
              }
            , Cmd.none
            )

        SaveSysReqToRegionalOptions regionName ->
            let
                app =
                    model.app

                ( newRegionalOptions, newEditingSysReq ) =
                    case model.sysReqToModify of
                        Just ( entryPointName, _, editingSysReq ) ->
                            case Dict.get regionName app.regionalOptions of
                                Just regionalOptions ->
                                    let
                                        newSysReqs =
                                            Dict.update
                                                entryPointName
                                                (\_ -> Just editingSysReq)
                                                regionalOptions.systemRequirements

                                        newOpts =
                                            { regionalOptions
                                                | systemRequirements = newSysReqs
                                            }

                                        newRegOptsDict =
                                            Dict.update
                                                regionName
                                                (\_ -> Just newOpts)
                                                app.regionalOptions
                                    in
                                    ( newRegOptsDict, Nothing )

                                _ ->
                                    ( app.regionalOptions, model.sysReqToModify )

                        _ ->
                            ( app.regionalOptions, model.sysReqToModify )

                newApp =
                    { app | regionalOptions = newRegionalOptions }
            in
            ( { model
                | app = newApp
                , sysReqToModify = newEditingSysReq
              }
            , Cmd.none
            )

        SaveTimeoutPolicy ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                timeoutPolicy =
                    Dict.toList runSpec.timeoutPolicy

                newTimeoutPolicy =
                    case model.timeoutPolicyToModify of
                        Just ( entryPointName, policy ) ->
                            case model.editingTimeoutPolicyIndex of
                                Just i ->
                                    updateAt i
                                        (\_ -> ( entryPointName, policy ))
                                        timeoutPolicy

                                _ ->
                                    timeoutPolicy
                                        ++ [ ( entryPointName, policy ) ]

                        _ ->
                            timeoutPolicy

                newRunSpec =
                    { runSpec | timeoutPolicy = Dict.fromList newTimeoutPolicy }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model
                | app = newApp
                , timeoutPolicyToModify = Nothing
                , editingTimeoutPolicyIndex = Nothing
              }
            , Cmd.none
            )

        SetExecDependsToModify index ->
            let
                ( newExecDepends, indexValue ) =
                    case getAt index model.app.runSpec.execDepends of
                        Just val ->
                            ( Just val, Just index )

                        _ ->
                            ( Nothing, Nothing )
            in
            ( { model
                | execDependsToModify = newExecDepends
                , editingExecDependsIndex = indexValue
              }
            , Cmd.none
            )

        SetInputSpecToModify index ->
            let
                ( newInputSpec, indexValue ) =
                    case getAt index model.app.inputSpec of
                        Just val ->
                            ( Just val, Just index )

                        _ ->
                            ( Nothing, Nothing )
            in
            ( { model
                | inputSpecToModify = newInputSpec
                , editingInputSpecIndex = indexValue
              }
            , Cmd.none
            )

        SetOutputSpecToModify index ->
            let
                ( newOutputSpec, indexValue ) =
                    case getAt index model.app.outputSpec of
                        Just val ->
                            ( Just val, Just index )

                        _ ->
                            ( Nothing, Nothing )
            in
            ( { model
                | outputSpecToModify = newOutputSpec
                , editingOutputSpecIndex = indexValue
              }
            , Cmd.none
            )

        SetRegionalOptionsToModify regionName ->
            let
                optToModify =
                    Maybe.map
                        (\opt -> ( regionName, opt ))
                        (Dict.get regionName model.app.regionalOptions)
            in
            ( { model | regionalOptionsToModify = optToModify }, Cmd.none )

        SetRegionalOptionsSysReqToModify regionName entryPointName sysReq ->
            --let
            --    regionalOpts =
            --        Dict.get regionName model.app.regionalOptions
            --    sysReq =
            --        Maybe.map
            --            (\d -> Dict.get entryPointName d.systemRequirements)
            --            regionalOpts
            --    _ =
            --        Debug.log "sysReq" sysReq
            --    s =
            --        case sysReq of
            --            Just (Just (Just x)) ->
            --                Just x
            --            _ ->
            --                Nothing
            --sysReqToModify =
            --    Maybe.map
            --        (\req ->
            --            ( entryPointName
            --            , Just regionName
            --            , s
            --            )
            --        )
            --        sysReq
            ( { model
                | sysReqToModify = Just ( entryPointName, Just regionName, sysReq )
              }
            , Cmd.none
            )

        SetRunSpecSysReqToModify entryPointName ->
            ( { model
                | sysReqToModify =
                    Maybe.map
                        (\val -> ( entryPointName, Nothing, val ))
                        (Dict.get
                            entryPointName
                            model.app.runSpec.systemRequirements
                        )
              }
            , Cmd.none
            )

        SetTimeoutPolicyToModify index ->
            let
                policies =
                    Dict.toList model.app.runSpec.timeoutPolicy

                ( newTimeoutPolicy, indexValue ) =
                    case getAt index policies of
                        Just val ->
                            ( Just val, Just index )

                        _ ->
                            ( Nothing, Nothing )
            in
            ( { model
                | timeoutPolicyToModify = newTimeoutPolicy
                , editingTimeoutPolicyIndex = indexValue
              }
            , Cmd.none
            )

        ShowJsonDialog ->
            ( { model | incomingJson = Just (encodeApp model.app) }
            , Cmd.none
            )

        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )

        UpdateAccessSpecDeveloper val ->
            let
                app =
                    model.app

                newAccess =
                    Maybe.map
                        (\access -> { access | developer = Just val })
                        app.access

                newApp =
                    { app | access = newAccess }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateAccessSpecProjectCreation val ->
            let
                app =
                    model.app

                newAccess =
                    Maybe.map
                        (\access -> { access | projectCreation = Just val })
                        app.access

                newApp =
                    { app | access = newAccess }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateInputSpecOptional val ->
            let
                newInputSpecToModify =
                    case model.inputSpecToModify of
                        Just spec ->
                            Just { spec | optional = val }

                        _ ->
                            Nothing
            in
            ( { model | inputSpecToModify = newInputSpecToModify }
            , Cmd.none
            )

        UpdateCustomCategory val ->
            ( { model | customCategory = val }, Cmd.none )

        UpdateAuthorizedUser val ->
            ( { model | authorizedUser = String.trim val }, Cmd.none )

        UpdateDeveloper val ->
            ( { model | developer = String.trim val }, Cmd.none )

        UpdateAddAuthorizedUser ->
            let
                app =
                    model.app

                newAuthorizedUsers =
                    if String.isEmpty model.authorizedUser then
                        app.authorizedUsers

                    else
                        mkSortedUniqList
                            app.authorizedUsers
                            model.authorizedUser

                newApp =
                    { app | authorizedUsers = newAuthorizedUsers }
            in
            ( { model | app = newApp, authorizedUser = "" }, Cmd.none )

        UpdateAddDeveloper ->
            let
                app =
                    model.app

                newDevelopers =
                    if String.isEmpty model.developer then
                        app.developers

                    else
                        mkSortedUniqList app.developers model.developer

                newApp =
                    { app | developers = newDevelopers }
            in
            ( { model | app = newApp, developer = "" }, Cmd.none )

        UpdateAppAddCustomCategory ->
            let
                app =
                    model.app

                customCat =
                    String.trim model.customCategory

                ( newCustomCat, newCats ) =
                    case String.isEmpty customCat of
                        True ->
                            ( customCat, app.categories )

                        False ->
                            ( "", mkSortedUniqList app.categories customCat )

                newApp =
                    { app | categories = newCats }
            in
            ( { model
                | customCategory = newCustomCat
                , app = newApp
              }
            , Cmd.none
            )

        UpdateAccessSpecNetwork val ->
            ( { model | accessSpecNetwork = val }, Cmd.none )

        UpdateAccessSpecAllProjects val ->
            let
                app =
                    model.app

                newAccess =
                    Maybe.map
                        (\access ->
                            { access
                                | allProjects = stringToMaybe (String.trim val)
                            }
                        )
                        app.access

                newApp =
                    { app | access = newAccess }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateAccessSpecProject val ->
            let
                app =
                    model.app

                newAccess =
                    Maybe.map
                        (\access ->
                            { access
                                | project = stringToMaybe (String.trim val)
                            }
                        )
                        app.access

                newApp =
                    { app | access = newAccess }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateAccessSpecAddNetwork ->
            let
                app =
                    model.app

                newAccess =
                    Maybe.map
                        (\access ->
                            { access
                                | network =
                                    List.sort access.network
                                        ++ [ model.accessSpecNetwork ]
                            }
                        )
                        app.access

                newApp =
                    { app | access = newAccess }
            in
            ( { model | accessSpecNetwork = "", app = newApp }, Cmd.none )

        UpdateAppCategories newCat ->
            let
                app =
                    model.app

                newCats =
                    case String.startsWith "--" newCat of
                        True ->
                            app.categories

                        False ->
                            mkSortedUniqList app.categories newCat

                newApp =
                    { app | categories = newCats }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateAppDescription val ->
            let
                app =
                    model.app

                newApp =
                    { app | description = val }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateAppDxapi val ->
            let
                app =
                    model.app

                newApp =
                    { app | dxapi = val }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateAppName val ->
            let
                newVal =
                    String.replace " " "_" (String.toLower val)

                app =
                    model.app

                badChars =
                    Maybe.withDefault Regex.never <|
                        Regex.fromString "[^a-z0-9_]"

                ( newName, err ) =
                    case Regex.contains badChars newVal of
                        True ->
                            ( app.name, Just "Disallowed character in name" )

                        _ ->
                            ( newVal, Nothing )

                newApp =
                    { app | name = newName }
            in
            ( { model | app = newApp, error = err }, Cmd.none )

        UpdateAppDeveloperNotes val ->
            let
                app =
                    model.app

                newApp =
                    { app | developerNotes = String.trim val }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateAppSummary val ->
            let
                app =
                    model.app

                newApp =
                    { app | summary = val }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateAppTitle val ->
            let
                app =
                    model.app

                newApp =
                    { app | title = val }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateAppVersion val ->
            let
                app =
                    model.app

                error =
                    case isSemanticVersion val of
                        True ->
                            Nothing

                        False ->
                            Just "Not a semantic version"

                newApp =
                    { app | version = val }
            in
            ( { model | error = error, app = newApp }, Cmd.none )

        UpdateExecDependsStage val ->
            ( { model | execDependsStage = val }, Cmd.none )

        UpdateExecDependsAddStage ->
            let
                app =
                    model.app

                newExec =
                    Maybe.map
                        (\exec ->
                            { exec
                                | stages =
                                    List.sort
                                        (exec.stages
                                            ++ [ model.execDependsStage ]
                                        )
                            }
                        )
                        model.execDependsToModify
            in
            ( { model | execDependsStage = "", execDependsToModify = newExec }
            , Cmd.none
            )

        UpdateExecDependsName val ->
            let
                newExecDependsToModify =
                    case model.execDependsToModify of
                        Just execDependsToModify ->
                            Just { execDependsToModify | name = val }

                        _ ->
                            Nothing
            in
            ( { model | execDependsToModify = newExecDependsToModify }
            , Cmd.none
            )

        UpdateExecDependsPackageManager val ->
            let
                newPackageManager =
                    case val of
                        "cpan" ->
                            Cpan

                        "cran" ->
                            Cran

                        "gem" ->
                            Gem

                        "pip" ->
                            Pip

                        _ ->
                            Apt

                newExecDependsToModify =
                    case model.execDependsToModify of
                        Just execDependsToModify ->
                            Just
                                { execDependsToModify
                                    | packageManager = newPackageManager
                                }

                        _ ->
                            Nothing
            in
            ( { model | execDependsToModify = newExecDependsToModify }
            , Cmd.none
            )

        UpdateExecDependsVersion val ->
            let
                newExecDependsToModify =
                    case model.execDependsToModify of
                        Just execDependsToModify ->
                            Just { execDependsToModify | version = val }

                        _ ->
                            Nothing
            in
            ( { model | execDependsToModify = newExecDependsToModify }
            , Cmd.none
            )

        UpdateHttpsAppAddPort val ->
            let
                app =
                    model.app

                newPorts httpsApp =
                    case String.toInt val of
                        Just newPort ->
                            mkSortedUniqList httpsApp.ports newPort

                        _ ->
                            httpsApp.ports

                newHttpsApp =
                    Maybe.map
                        (\httpsApp -> { httpsApp | ports = newPorts httpsApp })
                        app.httpsApp

                newApp =
                    { app | httpsApp = newHttpsApp }
            in
            ( { model | app = newApp, httpsAppPort = Nothing }, Cmd.none )

        UpdateHttpsAppSharedAccess val ->
            let
                app =
                    model.app

                newHttpsApp =
                    Maybe.map
                        (\httpsApp -> { httpsApp | sharedAccess = val })
                        app.httpsApp

                newApp =
                    { app | httpsApp = newHttpsApp }
            in
            ( { model | app = newApp }
            , Cmd.none
            )

        UpdateInputSpecName val ->
            let
                newName =
                    String.replace " " "_" val

                notAllowed =
                    Maybe.withDefault Regex.never <|
                        Regex.fromString "[^0-9a-zA-Z_]"

                cleaned =
                    Regex.replace notAllowed (\_ -> "") newName

                newInputSpecToModify =
                    case model.inputSpecToModify of
                        Just spec ->
                            Just { spec | name = cleaned }

                        _ ->
                            Nothing
            in
            ( { model | inputSpecToModify = newInputSpecToModify }
            , Cmd.none
            )

        UpdateInputSpecClass val ->
            let
                ( newType, newDefaultValue ) =
                    case val of
                        "applet" ->
                            ( InputSpecApplet, DefaultValString "" )

                        "array:applet" ->
                            ( InputSpecArrayApplet, DefaultValString "" )

                        "array:boolean" ->
                            ( InputSpecArrayBoolean, DefaultValString "" )

                        "array:file" ->
                            ( InputSpecArrayFile, DefaultValString "" )

                        "array:float" ->
                            ( InputSpecArrayFloat, DefaultValString "" )

                        "array:int" ->
                            ( InputSpecArrayInt, DefaultValString "" )

                        "array:record" ->
                            ( InputSpecArrayRecord, DefaultValString "" )

                        "array:string" ->
                            ( InputSpecArrayString, DefaultValString "" )

                        "boolean" ->
                            ( InputSpecBool, DefaultValBool False )

                        "file" ->
                            ( InputSpecFile, DefaultValString "" )

                        "float" ->
                            ( InputSpecFloat, DefaultValFloat 0.0 )

                        "hash" ->
                            ( InputSpecHash, DefaultValString "" )

                        "int" ->
                            ( InputSpecInt, DefaultValInt 0 )

                        "record" ->
                            ( InputSpecRecord, DefaultValString "" )

                        _ ->
                            ( InputSpecString, DefaultValString "" )

                newInputSpecToModify =
                    Maybe.map
                        (\spec ->
                            { spec
                                | class = newType
                                , default = newDefaultValue
                            }
                        )
                        model.inputSpecToModify
            in
            ( { model
                | inputSpecToModify = newInputSpecToModify
                , inputSpecToModifyDefault =
                    defaultValueToString newDefaultValue
              }
            , Cmd.none
            )

        UpdateInputSpecDefault val ->
            ( { model | inputSpecToModifyDefault = val }
            , Cmd.none
            )

        UpdateInputSpecHelp val ->
            let
                newInputSpecToModify =
                    case model.inputSpecToModify of
                        Just spec ->
                            Just { spec | help = val }

                        _ ->
                            Nothing
            in
            ( { model | inputSpecToModify = newInputSpecToModify }
            , Cmd.none
            )

        UpdateInputSpecChoice val ->
            ( { model | inputSpecChoice = String.trim val }
            , Cmd.none
            )

        UpdateInputSpecCustomPattern val ->
            ( { model | inputSpecPattern = String.trim val }
            , Cmd.none
            )

        UpdateInputSpecAddChoice ->
            let
                app =
                    model.app

                newChoice =
                    model.inputSpecChoice

                newInputSpecToModify =
                    Maybe.map
                        (\spec ->
                            { spec
                                | choices =
                                    List.Unique.toList <|
                                        List.Unique.fromList <|
                                            List.sort <|
                                                (spec.choices ++ [ newChoice ])
                            }
                        )
                        model.inputSpecToModify
            in
            ( { model
                | inputSpecToModify = newInputSpecToModify
                , inputSpecChoice = ""
              }
            , Cmd.none
            )

        UpdateInputSpecAddCustomPattern ->
            let
                app =
                    model.app

                newPat =
                    model.inputSpecPattern

                newInputSpecToModify =
                    Maybe.map
                        (\spec ->
                            { spec
                                | patterns =
                                    List.Unique.toList <|
                                        List.Unique.fromList <|
                                            List.sort <|
                                                (spec.patterns ++ [ newPat ])
                            }
                        )
                        model.inputSpecToModify
            in
            ( { model
                | inputSpecToModify = newInputSpecToModify
                , inputSpecPattern = ""
              }
            , Cmd.none
            )

        UpdateInputSpecAddPattern newPat ->
            let
                app =
                    model.app

                newInputSpecToModify =
                    Maybe.map
                        (\spec ->
                            { spec
                                | patterns =
                                    List.Unique.toList <|
                                        List.Unique.fromList <|
                                            List.sort <|
                                                (spec.patterns ++ [ newPat ])
                            }
                        )
                        model.inputSpecToModify
            in
            ( { model | inputSpecToModify = newInputSpecToModify }
            , Cmd.none
            )

        UpdateInputSpecLabel val ->
            let
                newInputSpecToModify =
                    Maybe.map
                        (\spec -> { spec | label = val })
                        model.inputSpecToModify
            in
            ( { model | inputSpecToModify = newInputSpecToModify }
            , Cmd.none
            )

        UpdateIncomingJson json ->
            ( { model | incomingJson = Just json }, Cmd.none )

        UpdateOutputSpecCustomPattern val ->
            ( { model | outputSpecPattern = String.trim val }
            , Cmd.none
            )

        UpdateOutputSpecAddPattern newPat ->
            let
                app =
                    model.app

                newOutputSpecToModify =
                    Maybe.map
                        (\spec ->
                            { spec
                                | patterns =
                                    List.sort (spec.patterns ++ [ newPat ])
                            }
                        )
                        model.outputSpecToModify
            in
            ( { model | outputSpecToModify = newOutputSpecToModify }
            , Cmd.none
            )

        UpdateOutputSpecAddCustomPattern ->
            let
                app =
                    model.app

                newPat =
                    model.outputSpecPattern

                newOutputSpecToModify =
                    Maybe.map
                        (\spec ->
                            { spec
                                | patterns =
                                    List.sort (spec.patterns ++ [ newPat ])
                            }
                        )
                        model.outputSpecToModify
            in
            ( { model
                | outputSpecToModify = newOutputSpecToModify
                , outputSpecPattern = ""
              }
            , Cmd.none
            )

        UpdateOutputSpecClass val ->
            let
                newOutputSpecToModify =
                    Maybe.map
                        (\spec -> { spec | class = val })
                        model.outputSpecToModify
            in
            ( { model | outputSpecToModify = newOutputSpecToModify }
            , Cmd.none
            )

        UpdateOutputSpecName val ->
            let
                newName =
                    String.replace " " "_" val

                notAllowed =
                    Maybe.withDefault Regex.never <|
                        Regex.fromString "[^0-9a-zA-Z_]"

                cleaned =
                    Regex.replace notAllowed (\_ -> "") newName

                newOutputSpecToModify =
                    case model.outputSpecToModify of
                        Just spec ->
                            Just { spec | name = cleaned }

                        _ ->
                            Nothing
            in
            ( { model | outputSpecToModify = newOutputSpecToModify }
            , Cmd.none
            )

        UpdateRegionalOptionsRegionName val ->
            let
                newOpts =
                    Maybe.map
                        (\( regionName, opts ) -> ( val, opts ))
                        model.regionalOptionsToModify
            in
            ( { model | regionalOptionsToModify = newOpts }, Cmd.none )

        UpdateRegionalOptionsResource val ->
            ( { model | regionalOptionsResource = val }, Cmd.none )

        UpdateRegionalOptionsAddResource ->
            let
                newOpts =
                    Maybe.map
                        (\( regionName, opts ) ->
                            let
                                curResources =
                                    case opts.resources of
                                        ResourceString s ->
                                            [ s ]

                                        ResourceList vals ->
                                            vals

                                newResources =
                                    ResourceList
                                        (curResources
                                            ++ [ model.regionalOptionsResource ]
                                        )
                            in
                            ( regionName, { opts | resources = newResources } )
                        )
                        model.regionalOptionsToModify
            in
            ( { model
                | regionalOptionsToModify = newOpts
                , regionalOptionsResource = ""
              }
            , Cmd.none
            )

        UpdateRunSpecInterpreter val ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                newInterpreter =
                    case val of
                        "python2.7" ->
                            Python27

                        "python3" ->
                            Python3

                        _ ->
                            Bash

                newRunSpec =
                    { runSpec | interpreter = newInterpreter }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateRunSpecDistribution val ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                newRunSpec =
                    { runSpec | distribution = val }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateRunSpecRelease val ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                newRunSpec =
                    { runSpec | release = val, interpreter = Bash }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateRunSpecRestartableEntryPoints val ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                newRunSpec =
                    { runSpec | restartableEntryPoints = val }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateRunSpecVersion val ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                newRunSpec =
                    { runSpec | version = val }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateRunSpecFile val ->
            let
                app =
                    model.app

                runSpec =
                    app.runSpec

                newRunSpec =
                    { runSpec | file = val }

                newApp =
                    { app | runSpec = newRunSpec }
            in
            ( { model | app = newApp }, Cmd.none )

        UpdateSysReqMinGpus val ->
            ( { model
                | editingSysReqMinGpus = Maybe.withDefault 0 (String.toInt val)
              }
            , Cmd.none
            )

        UpdateSysReqMinCores val ->
            ( { model
                | editingSysReqMinCores = Maybe.withDefault 0 (String.toInt val)
              }
            , Cmd.none
            )

        UpdateSysReqMinMemory val ->
            ( { model
                | editingSysReqMinMemory =
                    Maybe.withDefault 0 (String.toInt val)
              }
            , Cmd.none
            )

        UpdateSysReqHost val ->
            let
                newHost =
                    case val of
                        "Azure" ->
                            Azure

                        _ ->
                            Aws

                newInstanceType =
                    case newHost of
                        Aws ->
                            "mem2_hdd2_x2"

                        Azure ->
                            "azure:mem1_ssd1_x2"

                newSysReq =
                    Maybe.map
                        (\( entryPointName, regionName, sysReq ) ->
                            ( entryPointName
                            , regionName
                            , { sysReq | instanceType = newInstanceType }
                            )
                        )
                        model.sysReqToModify
            in
            ( { model
                | editingSysReqHost = Just newHost
                , sysReqToModify = newSysReq
              }
            , Cmd.none
            )

        UpdateSysReqEnableCluster enabled ->
            let
                app =
                    model.app

                newClusterSpec =
                    if enabled then
                        Just initialClusterSpec

                    else
                        Nothing

                newSysReq =
                    Maybe.map
                        (\( entryPointName, regionName, spec ) ->
                            ( entryPointName
                            , regionName
                            , { spec | clusterSpec = newClusterSpec }
                            )
                        )
                        model.sysReqToModify
            in
            ( { model
                | enableSysReqClusterDefinition = enabled
                , sysReqToModify = newSysReq
              }
            , Cmd.none
            )

        UpdateSysReqEntryPointName val ->
            let
                newSysReq =
                    Maybe.map
                        (\( entryPointName, regionName, sysReq ) ->
                            ( val, regionName, sysReq )
                        )
                        model.sysReqToModify
            in
            ( { model | sysReqToModify = newSysReq }, Cmd.none )

        UpdateSysReqInstanceType val ->
            let
                newSysReq =
                    Maybe.map
                        (\( entryPointName, regionName, sysReq ) ->
                            ( entryPointName
                            , regionName
                            , { sysReq | instanceType = val }
                            )
                        )
                        model.sysReqToModify
            in
            ( { model | sysReqToModify = newSysReq }, Cmd.none )

        UpdateSysReqClusterSpecType val ->
            let
                newSysReq =
                    Maybe.map
                        (\( entryPointName, regionName, sysReq ) ->
                            let
                                newClusterSpec =
                                    Maybe.map
                                        (\spec ->
                                            { spec | type_ = val, version = "" }
                                        )
                                        sysReq.clusterSpec
                            in
                            ( entryPointName
                            , regionName
                            , { sysReq | clusterSpec = newClusterSpec }
                            )
                        )
                        model.sysReqToModify
            in
            ( { model | sysReqToModify = newSysReq }, Cmd.none )

        UpdateSysReqClusterSpecInitialInstanceCount val ->
            let
                newCount =
                    Maybe.withDefault 1 (String.toInt val)

                newSysReq =
                    Maybe.map
                        (\( entryPointName, regionName, sysReq ) ->
                            let
                                newClusterSpec =
                                    Maybe.map
                                        (\spec ->
                                            { spec
                                                | initialInstanceCount = newCount
                                            }
                                        )
                                        sysReq.clusterSpec
                            in
                            ( entryPointName
                            , regionName
                            , { sysReq | clusterSpec = newClusterSpec }
                            )
                        )
                        model.sysReqToModify
            in
            ( { model | sysReqToModify = newSysReq }, Cmd.none )

        UpdateSysReqClusterSpecBootstrapScript val ->
            let
                newSysReq =
                    Maybe.map
                        (\( entryPointName, regionName, sysReq ) ->
                            let
                                newClusterSpec =
                                    Maybe.map
                                        (\spec ->
                                            { spec | bootstrapScript = val }
                                        )
                                        sysReq.clusterSpec
                            in
                            ( entryPointName
                            , regionName
                            , { sysReq | clusterSpec = newClusterSpec }
                            )
                        )
                        model.sysReqToModify
            in
            ( { model | sysReqToModify = newSysReq }, Cmd.none )

        UpdateSysReqClusterSpecPorts val ->
            let
                newSysReq =
                    Maybe.map
                        (\( entryPointName, regionName, sysReq ) ->
                            let
                                newClusterSpec =
                                    Maybe.map
                                        (\spec -> { spec | ports = val })
                                        sysReq.clusterSpec
                            in
                            ( entryPointName
                            , regionName
                            , { sysReq | clusterSpec = newClusterSpec }
                            )
                        )
                        model.sysReqToModify
            in
            ( { model | sysReqToModify = newSysReq }, Cmd.none )

        UpdateSysReqClusterSpecVersion val ->
            let
                newSysReq =
                    Maybe.map
                        (\( entryPointName, regionName, sysReq ) ->
                            let
                                newClusterSpec =
                                    Maybe.map
                                        (\spec -> { spec | version = val })
                                        sysReq.clusterSpec
                            in
                            ( entryPointName
                            , regionName
                            , { sysReq | clusterSpec = newClusterSpec }
                            )
                        )
                        model.sysReqToModify
            in
            ( { model | sysReqToModify = newSysReq }, Cmd.none )

        UpdateTimeoutPolicyEntryPointName val ->
            let
                newTimeout =
                    Maybe.map
                        (\( entryPointName, policy ) -> ( val, policy ))
                        model.timeoutPolicyToModify
            in
            ( { model | timeoutPolicyToModify = newTimeout }, Cmd.none )

        UpdateTimeoutPolicyHours val ->
            let
                trimmed =
                    String.trim val

                newVal =
                    if String.isEmpty trimmed then
                        "0"

                    else
                        trimmed

                newTimeout =
                    Maybe.map
                        (\( entryPointName, timeout ) ->
                            let
                                newHours =
                                    Maybe.withDefault
                                        timeout.hours
                                        (String.toInt newVal)
                            in
                            ( entryPointName
                            , { timeout | hours = newHours }
                            )
                        )
                        model.timeoutPolicyToModify
            in
            ( { model | timeoutPolicyToModify = newTimeout }, Cmd.none )

        UpdateTimeoutPolicyMinutes val ->
            let
                trimmed =
                    String.trim val

                newVal =
                    if String.isEmpty trimmed then
                        "0"

                    else
                        trimmed

                newTimeout =
                    Maybe.map
                        (\( entryPointName, timeout ) ->
                            let
                                newMinutes =
                                    Maybe.withDefault
                                        timeout.minutes
                                        (String.toInt newVal)
                            in
                            ( entryPointName
                            , { timeout | minutes = newMinutes }
                            )
                        )
                        model.timeoutPolicyToModify
            in
            ( { model | timeoutPolicyToModify = newTimeout }, Cmd.none )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Tab.subscriptions model.tabState TabMsg



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        title =
            "The Appetizer"

        err =
            case model.error of
                Nothing ->
                    div [] [ text "" ]

                Just e ->
                    div [ class "alert alert-danger" ]
                        [ text ("Error: " ++ e) ]
    in
    { title = title
    , body =
        [ Grid.container []
            [ h1 [] [ text title ]
            , err
            , Tab.config TabMsg
                |> Tab.withAnimation
                |> Tab.right
                |> Tab.items
                    [ Tab.item
                        { id = "tabMain"
                        , link = Tab.link [] [ text "Main" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneMain model
                                ]
                        }
                    , Tab.item
                        { id = "tabRunSpec"
                        , link = Tab.link [] [ text "RunSpec" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneRunSpec model
                                ]
                        }
                    , Tab.item
                        { id = "tabTimeoutPolicy"
                        , link = Tab.link [] [ text "Timeout" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneTimeoutPolicy model
                                ]
                        }
                    , Tab.item
                        { id = "tabExecDepends"
                        , link = Tab.link [] [ text "ExecDepends" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneExecDepends model
                                ]
                        }
                    , Tab.item
                        { id = "tabInputSpec"
                        , link = Tab.link [] [ text "Input" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneInputSpec model
                                ]
                        }
                    , Tab.item
                        { id = "tabOutputSpec"
                        , link = Tab.link [] [ text "Output" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneOutputSpec model
                                ]
                        }
                    , Tab.item
                        { id = "tabAccessSpec"
                        , link = Tab.link [] [ text "Access" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneAccessSpec model
                                ]
                        }
                    , Tab.item
                        { id = "tabUsers"
                        , link = Tab.link [] [ text "Users" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneUsers model
                                ]
                        }
                    , Tab.item
                        { id = "tabHttpsApp"
                        , link = Tab.link [] [ text "HttpsApp" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneHttpsApp model
                                ]
                        }
                    , Tab.item
                        { id = "tabRegionalOptions"
                        , link = Tab.link [] [ text "RegOpts" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneRegionalOptions model
                                ]
                        }
                    , Tab.item
                        { id = "tabDebug"
                        , link = Tab.link [] [ text "Debug" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneDebug model
                                ]
                        }
                    , Tab.item
                        { id = "tabJson"
                        , link = Tab.link [] [ text "JSON" ]
                        , pane =
                            Tab.pane []
                                [ br [] []
                                , paneJson model
                                ]
                        }
                    ]
                |> Tab.view model.tabState
            ]

        -- , div [] [ text (Debug.toString model) ]
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


paneMain : Model -> Html Msg
paneMain model =
    let
        app =
            model.app

        mkLi val =
            li []
                [ text val
                , Button.button
                    [ Button.small
                    , Button.light
                    , Button.onClick (DeleteAppCategory val)
                    ]
                    [ text "Delete" ]
                ]

        curCats =
            case List.isEmpty app.categories of
                True ->
                    text "None"

                _ ->
                    ul [] (List.map mkLi app.categories)
    in
    Grid.container []
        [ Form.form []
            [ mkFormRowTextEntry "Name" app.name UpdateAppName
            , mkFormRowTextEntry "Title" app.title UpdateAppTitle
            , mkFormRowTextEntry "DX API Version" app.dxapi UpdateAppDxapi
            , mkFormRowTextEntry "App Version" app.version UpdateAppVersion
            , mkFormRowTextEntry "Summary" app.summary UpdateAppSummary
            , mkFormRowTextEntrySubmit
                "Add Category"
                model.customCategory
                UpdateCustomCategory
                UpdateAppAddCustomCategory
            ]
        , mkFormRowSelect
            "Select Category"
            ([ "--Select--" ] ++ validCategories)
            ""
            UpdateAppCategories
        , mkFormRowHtml "Categories" curCats
        , mkFormRowTextArea
            "Description (opt)"
            app.description
            UpdateAppDescription
        , mkFormRowTextArea "Developer Notes (opt)"
            app.developerNotes
            UpdateAppDeveloperNotes
        ]


paneDebug : Model -> Html Msg
paneDebug model =
    div []
        [ text (Debug.toString model) ]


paneTimeoutPolicy : Model -> Html Msg
paneTimeoutPolicy model =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ Button.button
                    [ Button.secondary
                    , Button.onClick
                        (ModifyTimeoutPolicyDialog "*" initialTimeoutPolicy Nothing)
                    ]
                    [ text "Add Timeout" ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ timeoutPolicyTable model.app.runSpec.timeoutPolicy ]
            ]
        , Grid.row []
            [ Grid.col []
                [ modifyTimeoutPolicyDialog model ]
            ]
        ]


paneRunSpec : Model -> Html Msg
paneRunSpec model =
    let
        runSpec =
            model.app.runSpec

        appExecutionEnvVersions =
            case runSpec.release of
                "20.04" ->
                    [ "0" ]

                _ ->
                    [ "0", "1" ]

        intepreterChoices =
            -- https://documentation.dnanexus.com/developer/apps/
            -- execution-environment
            -- #choosing-an-application-execution-environment
            case ( runSpec.release, runSpec.version ) of
                ( "16.04", "0" ) ->
                    [ Bash, Python27 ]

                _ ->
                    [ Bash, Python3 ]
    in
    Grid.container []
        [ Form.form []
            [ mkFormRowSelect "Interpreter"
                (List.map runSpecInterpreterToString intepreterChoices)
                (runSpecInterpreterToString runSpec.interpreter)
                UpdateRunSpecInterpreter
            , mkFormRowTextEntry "File"
                runSpec.file
                UpdateRunSpecFile
            , mkFormRowSelect "Distribution"
                [ "Ubuntu" ]
                runSpec.distribution
                UpdateRunSpecDistribution
            , mkFormRowSelect "Release"
                [ "20.04", "16.04" ]
                runSpec.release
                UpdateRunSpecRelease
            , mkFormRowSelect "Application Execution Version"
                appExecutionEnvVersions
                runSpec.version
                UpdateRunSpecVersion
            , mkFormRowSelect "Restartable Entry Points"
                [ "master", "all" ]
                runSpec.restartableEntryPoints
                UpdateRunSpecRestartableEntryPoints
            ]
        , Grid.row []
            [ Grid.col []
                [ h2 [] [ text "System Requirements" ] ]
            , Grid.col []
                [ Button.button
                    [ Button.onClick
                        (ModifySysReqDialog "*" Nothing initialSystemRequirements)
                    ]
                    [ text "Add SysReq" ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ modifySysReqDialog model SaveSysReqToRunSpec ]
            ]
        , Grid.row []
            [ Grid.col []
                [ systemsRequirementsTable
                    model.app.runSpec.systemRequirements
                ]
            ]
        ]


paneExecDepends : Model -> Html Msg
paneExecDepends model =
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.sm10 ]
                [ Button.button
                    [ Button.onClick
                        (ModifyExecDependsDialog initialExecDepends Nothing)
                    ]
                    [ text "Add Dependency" ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ execDependsTable model.app.runSpec.execDepends ]
            ]
        , Grid.row []
            [ Grid.col []
                [ modifyExecDependsDialog model ]
            ]
        ]


paneInputSpec : Model -> Html Msg
paneInputSpec model =
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.sm10 ]
                [ Button.button
                    [ Button.onClick
                        (ModifyInputSpecDialog initialInputSpec Nothing)
                    ]
                    [ text "Add Input" ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ inputSpecTable model.app.inputSpec ]
            ]
        , Grid.row []
            [ Grid.col []
                [ modifyInputSpecDialog model ]
            ]
        ]


paneOutputSpec : Model -> Html Msg
paneOutputSpec model =
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.sm10 ]
                [ Button.button
                    [ Button.onClick
                        (ModifyOutputSpecDialog initialOutputSpec Nothing)
                    ]
                    [ text "Add Output" ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ outputSpecTable model.app.outputSpec ]
            ]
        , Grid.row []
            [ Grid.col []
                [ modifyOutputSpecDialog model ]
            ]
        ]


paneAccessSpec : Model -> Html Msg
paneAccessSpec model =
    let
        app =
            model.app

        mkLi val =
            li []
                [ text val
                , Button.button
                    [ Button.small
                    , Button.light
                    , Button.onClick (DeleteAccessSpecNetwork val)
                    ]
                    [ text "Delete" ]
                ]

        curNetworks spec =
            case List.isEmpty spec.network of
                True ->
                    text "None"

                _ ->
                    ul [] (List.map mkLi spec.network)

        form =
            case app.access of
                Just spec ->
                    [ Form.form []
                        [ mkFormRowHtml "Allowed Networks" (curNetworks spec)
                        , mkFormRowTextEntrySubmit
                            "Add Network"
                            model.accessSpecNetwork
                            UpdateAccessSpecNetwork
                            UpdateAccessSpecAddNetwork
                        , mkFormRowSelect
                            "Project"
                            validAccessSpecOptions
                            (Maybe.withDefault "" spec.project)
                            UpdateAccessSpecProject
                        , mkFormRowSelect
                            "All Projects"
                            validAccessSpecOptions
                            (Maybe.withDefault "" spec.allProjects)
                            UpdateAccessSpecAllProjects
                        , mkFormRowCheckbox
                            "Developer"
                            (Maybe.withDefault False spec.developer)
                            UpdateAccessSpecDeveloper
                        , mkFormRowCheckbox
                            "Project Creation"
                            (Maybe.withDefault False spec.projectCreation)
                            UpdateAccessSpecProjectCreation
                        ]
                    ]

                _ ->
                    []
    in
    Grid.container []
        ([ mkFormRowCheckbox "Enable" model.enableAccess EnableAccess ]
            ++ form
        )


paneJson : Model -> Html Msg
paneJson model =
    div []
        [ div [ style "text-align" "center" ]
            [ button [ class "btn btn-primary", onClick ShowJsonDialog ]
                [ text "Manual Edit" ]
            ]
        , br [] []
        , pre [] [ text (encodeApp model.app) ]
        , modifyJsonDialog model
        ]


paneHttpsApp : Model -> Html Msg
paneHttpsApp model =
    let
        app =
            model.app

        mkLi index val =
            li []
                [ text val
                , Button.button
                    [ Button.small
                    , Button.light
                    , Button.onClick (DeleteHttpsAppPort index)
                    ]
                    [ text "Delete" ]
                ]

        curPorts ports =
            case List.isEmpty ports of
                True ->
                    text "No ports"

                _ ->
                    ul []
                        (List.indexedMap mkLi (List.map String.fromInt ports))

        form =
            case app.httpsApp of
                Just httpsApp ->
                    let
                        ports =
                            httpsApp.ports
                    in
                    [ Form.form []
                        [ mkFormRowSelect
                            "Add Port"
                            (List.map String.fromInt validHttpsPorts)
                            ""
                            UpdateHttpsAppAddPort
                        , mkFormRowHtml "Ports" (curPorts httpsApp.ports)
                        , mkFormRowSelect
                            "Shared Access"
                            [ "VIEW", "CONTRIBUTE", "ADMINISTER", "NONE" ]
                            httpsApp.sharedAccess
                            UpdateHttpsAppSharedAccess
                        ]
                    ]

                _ ->
                    []
    in
    Grid.container []
        ([ mkFormRowCheckbox "Enable" model.enableHttpsApp EnableHttpsApp ]
            ++ form
        )


paneRegionalOptions : Model -> Html Msg
paneRegionalOptions model =
    let
        opts =
            model.app.regionalOptions
    in
    div [ class "form-group", style "text-align" "center" ]
        [ button
            [ type_ "button"
            , class "btn btn-default"
            , onClick
                (ModifyRegionalOptionsDialog
                    "*"
                    initialRegionalOptions
                    Nothing
                )
            ]
            [ text "Add Options" ]
        , modifyRegionalOptionsDialog model
        , regionalOptionsTable model
        ]


paneUsers : Model -> Html Msg
paneUsers model =
    let
        developers =
            model.app.developers

        authorizedUsers =
            model.app.authorizedUsers

        mkLi f val =
            li []
                [ text val
                , Button.button
                    [ Button.small, Button.light, Button.onClick (f val) ]
                    [ text "Delete" ]
                ]

        curDevelopers =
            case List.isEmpty developers of
                True ->
                    text "None"

                _ ->
                    ul [] (List.map (mkLi DeleteDeveloper) developers)

        curAuthorizedUsers =
            case List.isEmpty authorizedUsers of
                True ->
                    text "None"

                _ ->
                    ul [] (List.map (mkLi DeleteAuthorizedUser) authorizedUsers)
    in
    Grid.container []
        [ Form.form []
            [ mkFormRowTextEntrySubmit
                "Add Developer"
                model.developer
                UpdateDeveloper
                UpdateAddDeveloper
            , mkFormRowHtml "Developers" curDevelopers
            , mkFormRowTextEntrySubmit
                "Add Authorized User"
                model.authorizedUser
                UpdateAuthorizedUser
                UpdateAddAuthorizedUser
            , mkFormRowHtml "Authorized Users" curAuthorizedUsers
            ]
        ]


mkFormRowText : String -> String -> Html Msg
mkFormRowText label defValue =
    Form.row []
        [ Form.colLabel [ Col.sm2 ] [ text label ]
        , Form.col [ Col.sm10 ]
            [ text defValue ]
        ]


mkFormRowTextEntry : String -> String -> (String -> Msg) -> Html Msg
mkFormRowTextEntry label defValue msg =
    Form.row []
        [ Form.colLabel [ Col.sm2 ] [ text label ]
        , Form.col [ Col.sm10 ]
            [ Input.text [ Input.value defValue, Input.onInput msg ] ]
        ]


mkFormRowNumberEntry : String -> String -> (String -> Msg) -> Html Msg
mkFormRowNumberEntry label defValue msg =
    Form.row []
        [ Form.colLabel [ Col.sm2 ] [ text label ]
        , Form.col [ Col.sm10 ]
            [ Input.number [ Input.value defValue, Input.onInput msg ] ]
        ]


mkFormRowTextArea : String -> String -> (String -> Msg) -> Html Msg
mkFormRowTextArea label defValue msg =
    Form.row []
        [ Form.colLabel [ Col.sm2 ] [ text label ]
        , Form.col [ Col.sm10 ]
            [ Textarea.textarea
                [ Textarea.value defValue
                , Textarea.onInput msg
                , Textarea.rows 10
                ]
            ]
        ]


mkFormRowTextEntrySubmit : String -> String -> (String -> Msg) -> Msg -> Html Msg
mkFormRowTextEntrySubmit label defValue inputMsg submitMsg =
    Form.row []
        [ Form.colLabel [ Col.sm2 ] [ text label ]
        , Form.col [ Col.sm8 ]
            [ Input.text [ Input.value defValue, Input.onInput inputMsg ] ]
        , Form.col [ Col.sm2 ]
            [ Button.button [ Button.onClick submitMsg ] [ text "Add" ] ]
        ]


mkFormRowHtml : String -> Html msg -> Html msg
mkFormRowHtml label html =
    Form.row []
        [ Form.colLabel [ Col.sm2 ] [ text label ]
        , Form.col [ Col.sm10 ]
            [ html ]
        ]


mkFormRowSelect : String -> List String -> String -> (String -> Msg) -> Html Msg
mkFormRowSelect label optList curOpt msg =
    let
        mkOption val =
            Select.item
                [ value val, selected (val == curOpt) ]
                [ text val ]
    in
    Form.row []
        [ Form.colLabel [ Col.sm2 ] [ text label ]
        , Form.col [ Col.sm10 ]
            [ Select.select [ Select.onChange msg ]
                (List.map mkOption optList)
            ]
        ]


mkFormRowCheckbox : String -> Bool -> (Bool -> Msg) -> Html Msg
mkFormRowCheckbox label state msg =
    Form.row []
        [ Form.colLabel [ Col.sm2 ] [ text label ]
        , Form.col [ Col.sm10 ]
            [ Checkbox.checkbox
                [ Checkbox.checked state
                , Checkbox.onCheck msg
                ]
                ""
            ]
        ]


mkTh : String -> Html msg
mkTh label =
    th [ style "align" "right" ] [ text label ]



--mkRowSelect : String -> List String -> String -> (String -> Msg) -> Html Msg
--mkRowSelect label optList curOpt msg =
--    let
--        mkOption val =
--            option [ value val, selected (val == curOpt) ] [ text val ]
--    in
--    tr []
--        [ mkTh label
--        , td []
--            [ select [ onInput msg ] (List.map mkOption optList) ]
--        ]


mkRowSelect : String -> List String -> String -> (String -> Msg) -> Html Msg
mkRowSelect label optList curOpt msg =
    let
        mkOption val =
            Select.item [ value val, selected (val == curOpt) ] [ text val ]
    in
    tr []
        [ mkTh label
        , td []
            [ Select.select [ Select.onChange msg ]
                (List.map mkOption optList)
            ]
        ]


mkRowText : String -> String -> Html Msg
mkRowText label defValue =
    tr []
        [ mkTh label
        , td [] [ text defValue ]
        ]


mkRowHtml : String -> Html msg -> Html msg
mkRowHtml label html =
    tr []
        [ mkTh label
        , td [] [ html ]
        ]


mkRowTextEntry : String -> String -> (String -> Msg) -> Html Msg
mkRowTextEntry label defValue msg =
    tr []
        [ mkTh label
        , td []
            [ input
                [ type_ "text"
                , value defValue
                , class "form-control"
                , onInput msg
                , size 60
                ]
                []
            ]
        ]


mkRowTextEntrySubmit : String -> String -> (String -> Msg) -> Msg -> Html Msg
mkRowTextEntrySubmit label defValue inputMsg submitMsg =
    tr []
        [ mkTh label
        , td []
            [ input
                [ type_ "text"
                , value defValue
                , class "form-control"
                , onInput inputMsg
                , size 60
                ]
                []
            ]
        , td []
            [ button
                [ class "btn btn-default"
                , type_ "button"
                , onClick submitMsg
                ]
                [ text "Add" ]
            ]
        ]


mkRowTextArea : String -> String -> (String -> Msg) -> Html Msg
mkRowTextArea label defValue msg =
    tr []
        [ mkTh label
        , td []
            [ textarea
                [ value defValue
                , class "form-control"
                , onInput msg
                , rows 10
                , cols 40
                ]
                []
            ]
        ]


mkRowCheckbox : String -> Bool -> Msg -> Html Msg
mkRowCheckbox label state msg =
    tr []
        [ mkTh label
        , td []
            [ input
                [ type_ "checkbox"
                , onClick msg
                , checked state
                , class "form-control"
                ]
                []
            ]
        ]


mkRowRadioButtonGroup : String -> List ( String, Bool, Msg ) -> Html Msg
mkRowRadioButtonGroup label options =
    tr []
        [ mkTh label
        , td []
            [ fieldset [] (List.map mkRadio options) ]
        ]


mkRadio : ( String, Bool, Msg ) -> Html Msg
mkRadio ( value, state, msg ) =
    label []
        [ input
            [ type_ "radio"
            , onClick msg
            , checked state
            , class "form-control"
            ]
            []
        , text value
        ]


execDependsTable : List ExecDepends -> Html Msg
execDependsTable execDepends =
    let
        inputTr index dep =
            tr []
                [ td [] [ text dep.name ]
                , td [] [ text (packageManagerToString dep.packageManager) ]
                , td [] [ text dep.version ]
                , td [] [ text (String.join ", " dep.stages) ]
                , td []
                    [ button
                        [ class "btn btn-default"
                        , onClick (SetExecDependsToModify index)
                        ]
                        [ text "Edit" ]
                    ]
                , td []
                    [ button
                        [ class "btn btn-default"
                        , onClick (DeleteExecDepends index)
                        ]
                        [ text "Delete" ]
                    ]
                ]

        tbl =
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Package Manager" ]
                        , th [] [ text "Version" ]
                        , th [] [ text "Stages" ]
                        , th [] [ text "" ]
                        , th [] [ text "" ]
                        ]
                    ]
                , tbody []
                    (List.indexedMap inputTr execDepends)
                ]
    in
    case List.isEmpty execDepends of
        True ->
            div [] [ text "No dependencies" ]

        False ->
            tbl


outputSpecTable : List OutputSpec -> Html Msg
outputSpecTable outputSpec =
    let
        mkRow index spec =
            Table.tr []
                [ Table.td [] [ text spec.name ]
                , Table.td [] [ text spec.class ]
                , Table.td []
                    [ Button.button
                        [ Button.primary
                        , Button.onClick (SetOutputSpecToModify index)
                        ]
                        [ text "Edit" ]
                    ]
                , Table.td []
                    [ Button.button
                        [ Button.danger
                        , Button.onClick (DeleteOutputSpec index)
                        ]
                        [ text "Delete" ]
                    ]
                ]

        tbl =
            Table.table
                { options = [ Table.striped, Table.hover ]
                , thead =
                    Table.simpleThead
                        [ Table.th [] [ text "Name" ]
                        , Table.th [] [ text "Class" ]
                        , Table.th [] [ text "" ]
                        , Table.th [] [ text "" ]
                        ]
                , tbody =
                    Table.tbody []
                        (List.indexedMap mkRow outputSpec)
                }
    in
    case List.isEmpty outputSpec of
        True ->
            div [] [ text "No output specs" ]

        False ->
            tbl


inputSpecTable : List InputSpec -> Html Msg
inputSpecTable inputSpecs =
    let
        mkRow index spec =
            Table.tr []
                [ Table.td [] [ text spec.name ]
                , Table.td [] [ text spec.label ]
                , Table.td [] [ text (inputSpecClassToString spec.class) ]
                , Table.td []
                    [ text
                        (if spec.optional then
                            "True"

                         else
                            "False"
                        )
                    ]
                , Table.td []
                    [ Button.button
                        [ Button.primary
                        , Button.onClick (SetInputSpecToModify index)
                        ]
                        [ text "Edit" ]
                    ]
                , Table.td []
                    [ Button.button
                        [ Button.danger
                        , Button.onClick (DeleteInputSpec index)
                        ]
                        [ text "Delete" ]
                    ]
                ]

        tbl =
            Table.table
                { options = [ Table.striped, Table.hover ]
                , thead =
                    Table.simpleThead
                        [ Table.th [] [ text "Name" ]
                        , Table.th [] [ text "Label" ]
                        , Table.th [] [ text "Class" ]
                        , Table.th [] [ text "Optional" ]
                        , Table.th [] [ text "" ]
                        , Table.th [] [ text "" ]
                        ]
                , tbody =
                    Table.tbody []
                        (List.indexedMap mkRow inputSpecs)
                }
    in
    case List.isEmpty inputSpecs of
        True ->
            div [] [ text "No input specs" ]

        False ->
            tbl


regionalOptionsTable : Model -> Html Msg
regionalOptionsTable model =
    let
        regionalOptions =
            model.app.regionalOptions

        sysReqRow regionName ( entryPointName, req ) =
            tr []
                [ td [] [ text entryPointName ]
                , td [] [ text req.instanceType ]
                , td []
                    [ button
                        [ class "btn btn-default"
                        , onClick
                            (SetRegionalOptionsSysReqToModify regionName entryPointName req)
                        ]
                        [ text "Edit" ]
                    ]
                , td []
                    [ button
                        [ class "btn btn-default"

                        --, onClick (SetRegionalOptionsToModify regionName)
                        ]
                        [ text "Delete" ]
                    ]
                ]

        sysReqTable regionName systemRequirements =
            case Dict.isEmpty systemRequirements of
                True ->
                    text "No requirements"

                _ ->
                    table [ class "table" ]
                        [ thead []
                            [ tr []
                                [ th [] [ text "Entry Name" ]
                                , th [] [ text "Resources" ]
                                , th [] [ text "" ]
                                , th [] [ text "" ]
                                ]
                            ]
                        , tbody []
                            (List.map (sysReqRow regionName)
                                (Dict.toList systemRequirements)
                            )
                        ]

        optDiv ( regionName, opts ) =
            div []
                [ tbl regionName opts
                , sysReqTable regionName opts.systemRequirements
                , modifySysReqDialog
                    model
                    (SaveSysReqToRegionalOptions regionName)
                ]

        displayResources res =
            case res of
                ResourceString s ->
                    s

                ResourceList vals ->
                    String.join ", " vals

        tbl regionName opts =
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Region Name" ]
                        , th [] [ text "Resources" ]

                        --, th [] [ text "" ]
                        --, th [] [ text "" ]
                        ]
                    ]
                , tbody []
                    [ tr
                        []
                        [ td [] [ text regionName ]
                        , td [] [ text (displayResources opts.resources) ]
                        , td []
                            [ button
                                [ class "btn btn-default"
                                , onClick (SetRegionalOptionsToModify regionName)
                                ]
                                [ text "Edit" ]
                            ]
                        , td []
                            [ button
                                [ class "btn btn-default"
                                , onClick (DeleteRegionalOptions regionName)
                                ]
                                [ text "Delete" ]
                            ]
                        , td []
                            [ button
                                [ class "btn btn-default"
                                , onClick
                                    (ModifySysReqDialog
                                        "*"
                                        Nothing
                                        initialSystemRequirements
                                    )
                                ]
                                [ text "Add SysReq" ]
                            ]
                        ]
                    ]
                ]

        body =
            List.map optDiv (Dict.toList regionalOptions)
    in
    case Dict.isEmpty regionalOptions of
        True ->
            div [] [ text "No regional options" ]

        False ->
            div [] body


systemsRequirementsTable : Dict String SystemRequirements -> Html Msg
systemsRequirementsTable reqs =
    let
        mkRow index ( entryPointName, req ) =
            Table.tr []
                [ Table.td [] [ text entryPointName ]
                , Table.td [] [ text req.instanceType ]
                , Table.td []
                    [ Button.button
                        [ Button.primary
                        , Button.onClick
                            (SetRunSpecSysReqToModify entryPointName)
                        ]
                        [ text "Edit" ]
                    ]
                , Table.td []
                    [ Button.button
                        [ Button.danger
                        , Button.onClick (DeleteSysReq index)
                        ]
                        [ text "Delete" ]
                    ]
                ]

        tbl =
            Table.table
                { options = [ Table.striped, Table.hover ]
                , thead =
                    Table.simpleThead
                        [ Table.th [] [ text "Entry Point" ]
                        , Table.th [] [ text "Instance Type" ]
                        , Table.th [] [ text "" ]
                        , Table.th [] [ text "" ]
                        ]
                , tbody =
                    Table.tbody []
                        (List.indexedMap mkRow (Dict.toList reqs))
                }
    in
    case Dict.isEmpty reqs of
        True ->
            Grid.row [] [ Grid.col [] [ text "No requirements" ] ]

        False ->
            Grid.row [] [ Grid.col [] [ tbl ] ]


timeoutPolicyTable : Dict String TimeoutPolicy -> Html Msg
timeoutPolicyTable policies =
    let
        mkRow index ( entryPointName, policy ) =
            Table.tr []
                [ Table.td [] [ text entryPointName ]
                , Table.td [] [ text <| String.fromInt policy.hours ]
                , Table.td [] [ text <| String.fromInt policy.minutes ]
                , Table.td []
                    [ Button.button
                        [ Button.primary
                        , Button.onClick (SetTimeoutPolicyToModify index)
                        ]
                        [ text "Edit" ]
                    ]
                , Table.td []
                    [ Button.button
                        [ Button.danger
                        , Button.onClick (DeleteTimeoutPolicy index)
                        ]
                        [ text "Delete" ]
                    ]
                ]

        tbl =
            Table.table
                { options = [ Table.striped, Table.hover ]
                , thead =
                    Table.simpleThead
                        [ Table.th [] [ text "Entry Point" ]
                        , Table.th [] [ text "Hours" ]
                        , Table.th [] [ text "Minutes" ]
                        , Table.th [] [ text "" ]
                        , Table.th [] [ text "" ]
                        ]
                , tbody =
                    Table.tbody []
                        (List.indexedMap mkRow (Dict.toList policies))
                }
    in
    case Dict.isEmpty policies of
        True ->
            div [] [ text "No timeout policies" ]

        False ->
            tbl


modifyExecDependsDialog : Model -> Html Msg
modifyExecDependsDialog model =
    let
        mkLi val =
            li []
                [ text val
                , button
                    [ type_ "button"
                    , class "btn btn-default"
                    , onClick (DeleteExecDependsStage val)
                    ]
                    [ text "Delete" ]
                ]

        curStages stages =
            case List.length stages > 0 of
                True ->
                    ul [] (List.map mkLi stages)

                _ ->
                    text "None"

        packageManagers =
            List.map packageManagerToString
                [ Apt, Cpan, Cran, Gem, Pip ]

        tbl execDepends =
            div []
                [ div
                    [ style "overflow-y" "auto"
                    , style "max-height" "60vh"
                    ]
                    [ Html.form []
                        [ table []
                            [ mkRowTextEntry "Name"
                                execDepends.name
                                UpdateExecDependsName
                            , mkRowSelect
                                "Package Manager"
                                packageManagers
                                (packageManagerToString
                                    execDepends.packageManager
                                )
                                UpdateExecDependsPackageManager
                            , mkRowTextEntry
                                "Version"
                                execDepends.version
                                UpdateExecDependsVersion
                            , mkRowHtml
                                "Stages"
                                (curStages execDepends.stages)
                            , mkRowTextEntrySubmit
                                "Add Stage"
                                model.execDependsStage
                                UpdateExecDependsStage
                                UpdateExecDependsAddStage
                            ]
                        ]
                    ]
                ]
    in
    Dialog.view <|
        Maybe.map
            (\input ->
                { closeMessage = Nothing
                , containerClass = Nothing
                , header = Just (text "Add Input")
                , body = Just (tbl input)
                , footer =
                    Just
                        (div
                            []
                            [ button
                                [ class "btn btn-primary"
                                , type_ "button"
                                , onClick SaveExecDepends
                                ]
                                [ text "Save" ]
                            , button
                                [ class "btn btn-default"
                                , type_ "button"
                                , onClick CloseExecDependsDialog
                                ]
                                [ text "Cancel" ]
                            ]
                        )
                }
            )
            model.execDependsToModify


modifyOutputSpecDialog : Model -> Html Msg
modifyOutputSpecDialog model =
    let
        mkLi val =
            li []
                [ text val
                , Button.button
                    [ Button.small
                    , Button.light
                    , Button.onClick (DeleteOutputSpecPattern val)
                    ]
                    [ text "Delete" ]
                ]

        curPatterns pats =
            case List.isEmpty pats of
                True ->
                    text "None"

                _ ->
                    ul [] (List.map mkLi pats)

        errDiv =
            case model.outputSpecToModifyError of
                Nothing ->
                    div [] [ text "" ]

                Just e ->
                    div [ class "alert alert-danger" ]
                        [ text ("Error: " ++ e) ]

        tbl spec =
            div
                [ style "overflow-y" "auto"
                , style "max-height" "60vh"
                ]
                [ Grid.container []
                    [ Form.form []
                        [ mkFormRowTextEntry "Name"
                            spec.name
                            UpdateOutputSpecName
                        , mkFormRowSelect
                            "Class"
                            [ "file", "string" ]
                            spec.class
                            UpdateOutputSpecClass
                        , mkFormRowTextEntrySubmit
                            "Pattern"
                            model.outputSpecPattern
                            UpdateOutputSpecCustomPattern
                            UpdateOutputSpecAddCustomPattern
                        , mkFormRowHtml
                            "Patterns"
                            (curPatterns spec.patterns)
                        ]
                    ]
                ]
    in
    Dialog.view <|
        Maybe.map
            (\spec ->
                { closeMessage = Nothing
                , containerClass = Nothing
                , header = Just (text "Add Input")
                , body = Just (tbl spec)
                , footer =
                    Just
                        (div
                            []
                            [ button
                                [ class "btn btn-primary"
                                , type_ "button"
                                , onClick SaveOutputSpec
                                ]
                                [ text "Save" ]
                            , button
                                [ class "btn btn-default"
                                , type_ "button"
                                , onClick CloseOutputSpecDialog
                                ]
                                [ text "Cancel" ]
                            ]
                        )
                }
            )
            model.outputSpecToModify


modifyInputSpecDialog : Model -> Html Msg
modifyInputSpecDialog model =
    let
        mkLi f val =
            li []
                [ text val
                , Button.button
                    [ Button.small
                    , Button.light
                    , Button.onClick (f val)
                    ]
                    [ text "Delete" ]
                ]

        curPatterns pats =
            case List.isEmpty pats of
                True ->
                    text "None"

                _ ->
                    ul [] (List.map (mkLi DeleteInputSpecPattern) pats)

        curChoices pats =
            case List.length pats > 0 of
                True ->
                    ul [] (List.map (mkLi DeleteInputSpecChoice) pats)

                _ ->
                    text "None"

        classes =
            [ InputSpecApplet
            , InputSpecArrayApplet
            , InputSpecArrayBoolean
            , InputSpecArrayFile
            , InputSpecArrayFloat
            , InputSpecArrayInt
            , InputSpecArrayRecord
            , InputSpecArrayString
            , InputSpecBool
            , InputSpecFile
            , InputSpecFloat
            , InputSpecHash
            , InputSpecInt
            , InputSpecRecord
            , InputSpecString
            ]

        errDiv =
            case model.inputSpecToModifyError of
                Nothing ->
                    div [] [ text "" ]

                Just e ->
                    div [ class "alert alert-danger" ]
                        [ text ("Error: " ++ e) ]

        tbl spec =
            div
                [ style "overflow-y" "auto"
                , style "max-height" "60vh"
                ]
                [ Grid.container []
                    [ Form.form []
                        [ mkFormRowTextEntry "Name"
                            spec.name
                            UpdateInputSpecName
                        , mkFormRowTextEntry
                            "Label"
                            spec.label
                            UpdateInputSpecLabel
                        , mkFormRowSelect
                            "Class"
                            (List.map inputSpecClassToString classes)
                            (inputSpecClassToString spec.class)
                            UpdateInputSpecClass
                        , mkFormRowTextEntrySubmit
                            "Set Default"
                            model.inputSpecToModifyDefault
                            UpdateInputSpecDefault
                            SaveInputSpecDefault
                        , mkFormRowText
                            "Default Value"
                            (defaultValueToString spec.default)
                        , mkFormRowTextEntry
                            "Label"
                            spec.label
                            UpdateInputSpecLabel
                        , mkFormRowSelect
                            "Select Pattern: "
                            ([ "--Select--" ] ++ commonInputSpecPatterns)
                            ""
                            UpdateInputSpecAddPattern
                        , mkFormRowTextEntrySubmit
                            "Add Pattern"
                            model.inputSpecPattern
                            UpdateInputSpecCustomPattern
                            UpdateInputSpecAddCustomPattern
                        , mkFormRowHtml
                            "Patterns"
                            (curPatterns spec.patterns)
                        , mkFormRowTextEntrySubmit
                            "Add Choice"
                            model.inputSpecChoice
                            UpdateInputSpecChoice
                            UpdateInputSpecAddChoice
                        , mkFormRowHtml
                            "Choices"
                            (curChoices spec.choices)
                        , mkFormRowTextArea
                            "Help"
                            spec.help
                            UpdateInputSpecHelp
                        , mkFormRowCheckbox
                            "Optional"
                            spec.optional
                            UpdateInputSpecOptional
                        ]
                    ]
                ]
    in
    Dialog.view <|
        Maybe.map
            (\spec ->
                { closeMessage = Nothing
                , containerClass = Nothing
                , header = Just (text "Add Input")
                , body = Just (tbl spec)
                , footer =
                    Just
                        (div
                            []
                            [ button
                                [ class "btn btn-primary"
                                , type_ "button"
                                , onClick SaveInputSpec
                                ]
                                [ text "Save" ]
                            , button
                                [ class "btn btn-default"
                                , type_ "button"
                                , onClick CloseInputSpecDialog
                                ]
                                [ text "Cancel" ]
                            ]
                        )
                }
            )
            model.inputSpecToModify


modifyJsonDialog : Model -> Html Msg
modifyJsonDialog model =
    let
        err =
            case model.jsonError of
                Nothing ->
                    div [] [ text "" ]

                Just e ->
                    div [ class "alert alert-danger" ]
                        [ text ("Error: " ++ e) ]

        body =
            div
                [ style "overflow-y" "auto"
                , style "max-height" "60vh"
                ]
                [ err
                , textarea
                    [ cols 80, rows 30, onInput UpdateIncomingJson ]
                    [ text (encodeApp model.app) ]
                ]
    in
    Dialog.view <|
        Maybe.map
            (\json ->
                { closeMessage = Nothing
                , containerClass = Nothing
                , header = Just (text "Edit JSON")
                , body = Just body
                , footer =
                    Just
                        (div
                            []
                            [ button
                                [ class "btn btn-primary"
                                , type_ "button"
                                , onClick DecodeIncomingJson
                                ]
                                [ text "Save" ]
                            , button
                                [ class "btn btn-default"
                                , type_ "button"
                                , onClick CloseJsonDialog
                                ]
                                [ text "Cancel" ]
                            ]
                        )
                }
            )
            model.incomingJson


modifyRegionalOptionsDialog : Model -> Html Msg
modifyRegionalOptionsDialog model =
    let
        mkLi val =
            li []
                [ text val
                , button
                    [ type_ "button"
                    , class "btn btn-default"
                    , onClick (DeleteRegionalOptionsResource val)
                    ]
                    [ text "Delete" ]
                ]

        curResources resources =
            let
                res =
                    case resources of
                        ResourceString s ->
                            [ s ]

                        ResourceList vals ->
                            vals
            in
            case List.isEmpty res of
                True ->
                    text "None"

                _ ->
                    ul [] (List.map mkLi res)

        tbl regionName options =
            div []
                [ div
                    [ style "overflow-y" "auto"
                    , style "max-height" "60vh"
                    ]
                    [ Html.form []
                        [ table []
                            [ mkRowSelect
                                "Region Name"
                                validRegions
                                regionName
                                UpdateRegionalOptionsRegionName
                            , mkRowTextEntrySubmit
                                "Add Resource"
                                model.regionalOptionsResource
                                UpdateRegionalOptionsResource
                                UpdateRegionalOptionsAddResource
                            , mkRowHtml
                                "Resources"
                                (curResources options.resources)
                            ]
                        ]
                    ]
                ]
    in
    Dialog.view <|
        Maybe.map
            (\( regionName, opts ) ->
                { closeMessage = Nothing
                , containerClass = Nothing
                , header = Just (text "Add Regional Options")
                , body = Just (tbl regionName opts)
                , footer =
                    Just
                        (div
                            []
                            [ button
                                [ class "btn btn-primary"
                                , type_ "button"
                                , onClick SaveRegionalOptions
                                ]
                                [ text "Save" ]
                            , button
                                [ class "btn btn-default"
                                , type_ "button"
                                , onClick CloseRegionalOptionsDialog
                                ]
                                [ text "Cancel" ]
                            ]
                        )
                }
            )
            model.regionalOptionsToModify


modifySysReqDialog model saveFunction =
    let
        curHost =
            Maybe.withDefault Aws model.editingSysReqHost

        hostNames =
            List.map sysReqHostToString [ Aws, Azure ]

        instanceTypeChoices =
            validInstanceTypes
                |> List.filter
                    (\instance -> instance.host == curHost)
                |> List.filter
                    (\instance -> instance.cores >= model.editingSysReqMinCores)
                |> List.filter
                    (\instance -> instance.gpus >= model.editingSysReqMinGpus)
                |> List.filter
                    (\instance ->
                        instance.memory
                            >= toFloat model.editingSysReqMinMemory
                    )
                |> List.map (\instance -> instance.name)
                |> List.sort

        clusterForm clusterSpec =
            let
                clusterVersionChoices spec =
                    case spec.type_ of
                        "generic" ->
                            []

                        _ ->
                            validClusterSpecVersions
            in
            case clusterSpec of
                Just spec ->
                    [ mkFormRowSelect "Cluster Type"
                        validClusterSpecType
                        spec.type_
                        UpdateSysReqClusterSpecType
                    , mkFormRowSelect "Cluster Version"
                        (clusterVersionChoices spec)
                        spec.version
                        UpdateSysReqClusterSpecVersion
                    , mkFormRowSelect "Initial Instance Count"
                        (List.map String.fromInt (List.range 1 10))
                        (String.fromInt
                            spec.initialInstanceCount
                        )
                        UpdateSysReqClusterSpecInitialInstanceCount
                    , mkFormRowTextEntry "Ports"
                        spec.ports
                        UpdateSysReqClusterSpecPorts
                    , mkFormRowTextEntry "Bootstrap Script"
                        spec.bootstrapScript
                        UpdateSysReqClusterSpecBootstrapScript
                    ]

                _ ->
                    []

        tbl entryPointName req =
            div []
                [ div
                    [ style "overflow-y" "auto"
                    , style "max-height" "60vh"
                    ]
                    [ Grid.container []
                        [ Form.form []
                            ([ mkFormRowTextEntry "Entry Point"
                                entryPointName
                                UpdateSysReqEntryPointName
                             , mkFormRowSelect "Host"
                                hostNames
                                (sysReqHostToString curHost)
                                UpdateSysReqHost
                             , mkFormRowNumberEntry "Min. Cores"
                                (String.fromInt model.editingSysReqMinCores)
                                UpdateSysReqMinCores
                             , mkFormRowNumberEntry "Min. Memory (GB)"
                                (String.fromInt model.editingSysReqMinMemory)
                                UpdateSysReqMinMemory
                             , mkFormRowNumberEntry "GPU"
                                (String.fromInt model.editingSysReqMinGpus)
                                UpdateSysReqMinGpus
                             , mkFormRowSelect "Instance Type"
                                instanceTypeChoices
                                req.instanceType
                                UpdateSysReqInstanceType
                             , mkFormRowCheckbox "Define Cluster Spec"
                                model.enableSysReqClusterDefinition
                                UpdateSysReqEnableCluster
                             ]
                                ++ clusterForm req.clusterSpec
                            )
                        ]
                    ]
                ]
    in
    Dialog.view <|
        Maybe.map
            (\( entryPointName, _, req ) ->
                { closeMessage = Nothing
                , containerClass = Nothing
                , header = Just (text "Add Req")
                , body = Just (tbl entryPointName req)
                , footer =
                    Just
                        (div
                            []
                            [ button
                                [ class "btn btn-primary"
                                , type_ "button"
                                , onClick saveFunction
                                ]
                                [ text "Save" ]
                            , button
                                [ class "btn btn-default"
                                , type_ "button"
                                , onClick CloseSysReqDialog
                                ]
                                [ text "Cancel" ]
                            ]
                        )
                }
            )
            model.sysReqToModify


modifyTimeoutPolicyDialog : Model -> Html Msg
modifyTimeoutPolicyDialog model =
    let
        tbl entryPointName policy =
            div
                [ style "overflow-y" "auto"
                , style "max-height" "60vh"
                ]
                [ Grid.container []
                    [ Form.form []
                        [ mkFormRowTextEntry "Entry Point Name"
                            entryPointName
                            UpdateTimeoutPolicyEntryPointName
                        , mkFormRowTextEntry "Hours"
                            (String.fromInt policy.hours)
                            UpdateTimeoutPolicyHours
                        , mkFormRowTextEntry "Minutes"
                            (String.fromInt policy.minutes)
                            UpdateTimeoutPolicyMinutes
                        ]
                    ]
                ]
    in
    Dialog.view <|
        Maybe.map
            (\( entryPointName, policy ) ->
                { closeMessage = Nothing
                , containerClass = Nothing
                , header = Just (text "Add Policy")
                , body = Just (tbl entryPointName policy)
                , footer =
                    Just
                        (div
                            []
                            [ button
                                [ class "btn btn-primary"
                                , type_ "button"
                                , onClick SaveTimeoutPolicy
                                ]
                                [ text "Save" ]
                            , button
                                [ class "btn btn-default"
                                , type_ "button"
                                , onClick CloseTimeoutPolicyDialog
                                ]
                                [ text "Cancel" ]
                            ]
                        )
                }
            )
            model.timeoutPolicyToModify


isSemanticVersion : String -> Bool
isSemanticVersion val =
    let
        pattern =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[0-9]+[.][0-9]+[.][0-9]+$"
    in
    List.length (Regex.find pattern val) == 1


mkSortedUniqList : List comparable -> comparable -> List comparable
mkSortedUniqList vals newVal =
    vals
        ++ [ newVal ]
        |> List.sort
        |> List.Unique.fromList
        |> List.Unique.toList



-- JSON EN/DECODING


decoderApp : Decoder App
decoderApp =
    Decode.succeed App
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "summary" Decode.string
        |> Pipeline.optional "description" Decode.string ""
        |> Pipeline.optional "dxapi" Decode.string ""
        |> Pipeline.optional "version" Decode.string ""
        |> Pipeline.optional "developerNotes" Decode.string ""
        |> Pipeline.optional "categories" (Decode.list Decode.string) []
        |> Pipeline.optional "developers" (Decode.list Decode.string) []
        |> Pipeline.optional "authorizedUsers" (Decode.list Decode.string) []
        |> Pipeline.required "runSpec" decoderRunSpec
        |> Pipeline.optional "inputSpec" (Decode.list decoderInputSpec) []
        |> Pipeline.optional "outputSpec" (Decode.list decoderOutputSpec) []
        |> Pipeline.optional "access"
            (decoderAccessSpec |> Decode.map Just)
            Nothing
        |> Pipeline.optional "httpsApp"
            (decoderHttpsApp |> Decode.map Just)
            Nothing
        |> Pipeline.optional "regionalOptions"
            decoderRegionalOptions
            Dict.empty


decoderAccessSpec : Decoder AccessSpec
decoderAccessSpec =
    Decode.succeed AccessSpec
        |> Pipeline.required "network"
            (Decode.list Decode.string)
        |> Pipeline.optional "project"
            (Decode.nullable Decode.string)
            Nothing
        |> Pipeline.optional "allProjects"
            (Decode.nullable Decode.string)
            Nothing
        |> Pipeline.optional "developer"
            (Decode.nullable Decode.bool)
            Nothing
        |> Pipeline.optional "projectCreation"
            (Decode.nullable Decode.bool)
            Nothing


decoderRegionalOptions : Decoder (Dict.Dict String RegionalOptions)
decoderRegionalOptions =
    Decode.dict decoderRegionalOptionsBody


decoderRegionalOptionsBody : Decoder RegionalOptions
decoderRegionalOptionsBody =
    Decode.succeed RegionalOptions
        |> Pipeline.optional
            "resources"
            decoderRegionOptionsResource
            (ResourceList [])
        |> Pipeline.optional
            "systemRequirements"
            decoderSystemRequirements
            Dict.empty


decoderRegionOptionsResource =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                (\str -> Decode.succeed (ResourceString str))
        , Decode.list Decode.string
            |> Decode.andThen
                (\vals -> Decode.succeed (ResourceList vals))
        ]


decoderHttpsApp : Decoder HttpsApp
decoderHttpsApp =
    Decode.succeed HttpsApp
        |> Pipeline.required "ports" (Decode.list Decode.int)
        |> Pipeline.optional "shared_access" Decode.string ""


decoderRunSpec : Decoder RunSpec
decoderRunSpec =
    Decode.succeed RunSpec
        |> Pipeline.required "interpreter" decoderRunSpecInterpreter
        |> Pipeline.required "file" Decode.string
        |> Pipeline.required "distribution" Decode.string
        |> Pipeline.required "release" Decode.string
        |> Pipeline.optional "version" Decode.string ""
        |> Pipeline.optional "restartableEntryPoints" Decode.string ""
        |> Pipeline.optional
            "execDepends"
            (Decode.list decoderExecDepends)
            []
        |> Pipeline.optional
            "timeoutPolicy"
            decoderTimeoutPolicy
            Dict.empty
        |> Pipeline.optional
            "systemRequirements"
            decoderSystemRequirements
            Dict.empty
        |> Pipeline.optional
            "assetDepends"
            (Decode.list decoderAssetDepends)
            []


decoderRunSpecInterpreter =
    Decode.string
        |> Decode.andThen
            (\str ->
                case String.toLower str of
                    "python2.7" ->
                        Decode.succeed Python27

                    "python3" ->
                        Decode.succeed Python3

                    _ ->
                        Decode.succeed Bash
            )


decoderExecDependsPackageManager =
    Decode.string
        |> Decode.andThen
            (\str ->
                case String.toLower str of
                    "cpan" ->
                        Decode.succeed Cpan

                    "cran" ->
                        Decode.succeed Cran

                    "gem" ->
                        Decode.succeed Gem

                    "pip" ->
                        Decode.succeed Pip

                    _ ->
                        Decode.succeed Apt
            )


decoderExecDepends : Decoder ExecDepends
decoderExecDepends =
    Decode.succeed ExecDepends
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional
            "package_manager"
            decoderExecDependsPackageManager
            Apt
        |> Pipeline.optional "version" Decode.string ""
        |> Pipeline.optional "stages" (Decode.list Decode.string) []


decoderOutputSpec : Decoder OutputSpec
decoderOutputSpec =
    Decode.succeed OutputSpec
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional "label" Decode.string ""
        |> Pipeline.optional "help" Decode.string ""
        |> Pipeline.optional "optional" Decode.bool True
        |> Pipeline.optional "class" Decode.string ""
        |> Pipeline.optional "patterns" (Decode.list Decode.string) []


decoderInputSpec : Decoder InputSpec
decoderInputSpec =
    Decode.succeed InputSpec
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional "label" Decode.string ""
        |> Pipeline.optional "class" decoderInputSpecClass InputSpecString
        |> Pipeline.optional "optional" Decode.bool True
        |> Pipeline.optional "default" decoderDefaultValue (DefaultValString "")
        |> Pipeline.optional "patterns" (Decode.list Decode.string) []
        |> Pipeline.optional "choices" (Decode.list Decode.string) []
        |> Pipeline.optional "type" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "help" Decode.string ""
        |> Pipeline.optional "group" Decode.string ""
        |> Pipeline.optional "suggestions"
            (Decode.list decoderInputSpecSuggestion)
            []


decoderInputSpecSuggestion : Decoder InputSpecSuggestion
decoderInputSpecSuggestion =
    Decode.succeed InputSpecSuggestion
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "project" Decode.string
        |> Pipeline.required "path" Decode.string
        |> Pipeline.optional "region" Decode.string ""


decoderTimeoutPolicy : Decoder (Dict.Dict String TimeoutPolicy)
decoderTimeoutPolicy =
    Decode.dict decoderTimeoutPolicyBody


decoderTimeoutPolicyBody : Decoder TimeoutPolicy
decoderTimeoutPolicyBody =
    Decode.succeed TimeoutPolicyTime
        |> Pipeline.optionalAt [ "hours" ] Decode.int 0
        |> Pipeline.optionalAt [ "minutes" ] Decode.int 0


decoderSystemRequirements : Decoder (Dict.Dict String SystemRequirements)
decoderSystemRequirements =
    Decode.dict decoderSystemRequirementsBody


decoderAssetDepends : Decoder AssetDepends
decoderAssetDepends =
    Decode.oneOf
        [ decoderAssetDependsById
            |> Decode.map (\val -> AssetDependsById { id = val })
        , decoderAssetDependsByName
            |> Decode.map (\val -> AssetDependsByName { name = val })
        ]



--decoderAssetDependsById : Decoder AssetDepends


decoderAssetDependsById =
    Decode.field "id" Decode.string



--decoderAssetDependsByName : Decoder AssetDepends


decoderAssetDependsByName =
    Decode.field "name" Decode.string



-- |> Decode.succeed AssetDependsById
--decoderAssetDependsByName : Decoder AssetDepends
--decoderAssetDependsByName =
--    Decode.succeed AssetDependsByName
--        |> Pipeline.required "name" Decode.string
--        |> Decode.andThen (\val -> { name = val })


decoderSystemRequirementsBody : Decoder SystemRequirements
decoderSystemRequirementsBody =
    Decode.succeed SystemRequirements
        |> Pipeline.required "instanceType" Decode.string
        |> Pipeline.optional
            "clusterSpec"
            (decoderClusterSpec |> Decode.map Just)
            Nothing


decoderClusterSpec : Decoder ClusterSpec
decoderClusterSpec =
    Decode.succeed ClusterSpec
        |> Pipeline.required "type" Decode.string
        |> Pipeline.optional "version" Decode.string ""
        |> Pipeline.optional "initialInstanceCount" Decode.int 1
        |> Pipeline.optional "ports" Decode.string ""
        |> Pipeline.optional "bootstrapScript" Decode.string ""


decoderDefaultValue =
    Decode.oneOf
        [ Decode.float
            |> Decode.andThen
                (\f -> Decode.succeed (DefaultValFloat f))
        , Decode.int
            |> Decode.andThen
                (\i -> Decode.succeed (DefaultValInt i))
        , Decode.bool
            |> Decode.andThen
                (\b -> Decode.succeed (DefaultValBool b))
        , Decode.string
            |> Decode.andThen
                (\s -> Decode.succeed (DefaultValString s))
        ]


decoderInputSpecClass =
    Decode.string
        |> Decode.andThen
            (\str ->
                case String.toLower str of
                    "applet" ->
                        Decode.succeed InputSpecApplet

                    "array:applet" ->
                        Decode.succeed InputSpecArrayApplet

                    "array:boolean" ->
                        Decode.succeed InputSpecArrayBoolean

                    "array:file" ->
                        Decode.succeed InputSpecArrayFile

                    "array:float" ->
                        Decode.succeed InputSpecArrayFloat

                    "array:int" ->
                        Decode.succeed InputSpecArrayInt

                    "array:record" ->
                        Decode.succeed InputSpecArrayRecord

                    "array:string" ->
                        Decode.succeed InputSpecArrayString

                    "boolean" ->
                        Decode.succeed InputSpecBool

                    "file" ->
                        Decode.succeed InputSpecFile

                    "float" ->
                        Decode.succeed InputSpecFloat

                    "hash" ->
                        Decode.succeed InputSpecHash

                    "int" ->
                        Decode.succeed InputSpecInt

                    "record" ->
                        Decode.succeed InputSpecRecord

                    "string" ->
                        Decode.succeed InputSpecString

                    _ ->
                        Decode.fail <| "Unknown param type: " ++ str
            )


decodeIncomingJson : Model -> Model
decodeIncomingJson model =
    let
        setDefault spec =
            let
                _ =
                    Debug.log "default name" spec.name

                _ =
                    Debug.log "default value" spec.default

                _ =
                    Debug.log "default class" spec.class

                newDefault =
                    case ( spec.default, spec.class ) of
                        ( DefaultValString _, InputSpecString ) ->
                            spec.default

                        ( DefaultValFloat _, InputSpecFloat ) ->
                            spec.default

                        ( DefaultValFloat f, InputSpecInt ) ->
                            DefaultValInt (round f)

                        ( DefaultValInt i, InputSpecFloat ) ->
                            DefaultValFloat (toFloat i)

                        ( DefaultValInt _, InputSpecInt ) ->
                            spec.default

                        ( DefaultValBool _, InputSpecBool ) ->
                            spec.default

                        ( DefaultValString s, InputSpecInt ) ->
                            Maybe.withDefault 0
                                (String.toInt s)
                                |> DefaultValInt

                        ( DefaultValString s, InputSpecFloat ) ->
                            Maybe.withDefault 0.0
                                (String.toFloat s)
                                |> DefaultValFloat

                        ( DefaultValString s, InputSpecBool ) ->
                            DefaultValBool (String.toUpper s == "TRUE")

                        _ ->
                            DefaultValString (defaultValueToString spec.default)
            in
            { spec | default = newDefault }

        ( newApp, err ) =
            case model.incomingJson of
                Just json ->
                    case Decode.decodeString decoderApp json of
                        Ok app ->
                            let
                                inputSpecWithDefaults =
                                    List.map setDefault app.inputSpec
                            in
                            ( { app | inputSpec = inputSpecWithDefaults }
                            , Nothing
                            )

                        Err e ->
                            ( model.app, Just (Decode.errorToString e) )

                _ ->
                    ( model.app, Just "No JSON" )
    in
    { model | app = newApp, jsonError = err }


stringToMaybe s =
    if String.isEmpty s then
        Nothing

    else
        Just s


dictToMaybe d =
    if Dict.isEmpty d then
        Nothing

    else
        Just d


listToMaybe vals =
    if List.isEmpty vals then
        Nothing

    else
        Just vals


encodeApp : App -> String
encodeApp app =
    let
        appDescription =
            if String.isEmpty app.description then
                app.name

            else
                app.description
    in
    JE.encode 4
        (Opt.objectMaySkip
            [ Opt.field JE.string ( "name", app.name )
            , Opt.field JE.string ( "title", app.title )
            , Opt.field JE.string ( "summary", app.summary )
            , Opt.field JE.string ( "description", appDescription )
            , Opt.field JE.string ( "dxapi", app.dxapi )
            , Opt.field JE.string ( "version", app.version )
            , Opt.optionalField JE.string
                ( "developerNotes", stringToMaybe app.developerNotes )
            , Opt.optionalField (JE.list JE.string)
                ( "categories", listToMaybe app.categories )
            , Opt.optionalField (JE.list JE.string)
                ( "developers", listToMaybe app.developers )
            , Opt.optionalField (JE.list JE.string)
                ( "authorizedUsers", listToMaybe app.authorizedUsers )
            , Opt.field encodeRunSpec ( "runSpec", app.runSpec )
            , Opt.field (JE.list encodeInputSpec)
                ( "inputSpec", app.inputSpec )
            , Opt.field (JE.list encodeOutputSpec)
                ( "outputSpec", app.outputSpec )
            , Opt.optionalField encodeAccessSpec ( "access", app.access )
            , Opt.optionalField encodeHttpsApp ( "httpsApp", app.httpsApp )
            , Opt.optionalField (JE.dict identity encodeRegionalOptions)
                ( "regionalOptions", dictToMaybe app.regionalOptions )
            ]
        )


encodeAccessSpec access =
    Opt.objectMaySkip
        [ Opt.field (JE.list JE.string) ( "network", access.network )
        , Opt.optionalField JE.string ( "project", access.project )
        , Opt.optionalField JE.string ( "allProjects", access.allProjects )
        , Opt.optionalField JE.bool ( "developer", access.developer )
        , Opt.optionalField JE.bool ( "projectCreation", access.projectCreation )
        ]


encodeExecDepends execDepends =
    Opt.objectMaySkip
        [ Opt.field JE.string ( "name", execDepends.name )
        , Opt.field JE.string
            ( "package_manager"
            , packageManagerToString execDepends.packageManager
            )
        , Opt.optionalField JE.string
            ( "version", stringToMaybe execDepends.version )
        , Opt.optionalField (JE.list JE.string)
            ( "stages", listToMaybe execDepends.stages )
        ]


encodeOutputSpec spec =
    Opt.objectMaySkip
        [ Opt.field JE.string ( "name", spec.name )
        , Opt.optionalField JE.string ( "label", stringToMaybe spec.label )
        , Opt.optionalField JE.string ( "help", stringToMaybe spec.help )
        , Opt.field JE.bool ( "optional", spec.optional )
        , Opt.field JE.string ( "class", spec.class )
        , Opt.optionalField (JE.list JE.string)
            ( "patterns", listToMaybe spec.patterns )
        ]


encodeTimeoutPolicyTime policy =
    JE.object
        [ ( "hours", JE.int policy.hours )
        , ( "minutes", JE.int policy.minutes )
        ]


encodeClusterSpec spec =
    JE.object
        [ ( "type", JE.string spec.type_ )
        , ( "version", JE.string spec.version )
        , ( "initialInstanceCount", JE.int spec.initialInstanceCount )
        , ( "ports", JE.string spec.ports )
        , ( "bootstrapScript", JE.string spec.bootstrapScript )
        ]


encodeHttpsApp httpsApp =
    JE.object
        [ ( "ports", JE.list JE.int httpsApp.ports )
        , ( "shared_access", JE.string httpsApp.sharedAccess )
        ]


encodeRegionalOptions opts =
    let
        resources =
            case opts.resources of
                ResourceString s ->
                    [ s ]

                ResourceList vals ->
                    vals
    in
    Opt.objectMaySkip
        [ Opt.field (JE.dict identity encodeSystemRequirements)
            ( "systemRequirements", opts.systemRequirements )
        , Opt.optionalField (JE.list JE.string)
            ( "resources", listToMaybe resources )
        ]


encodeSystemRequirements req =
    Opt.objectMaySkip
        [ Opt.field JE.string ( "instanceType", req.instanceType )
        , Opt.optionalField encodeClusterSpec ( "clusterSpec", req.clusterSpec )
        ]


encodeAssetDepends assetDepends =
    case assetDepends of
        AssetDependsById asset ->
            JE.object
                [ ( "id", JE.string asset.id )
                ]

        AssetDependsByName asset ->
            JE.object
                [ ( "name", JE.string asset.name )
                ]


encodeRunSpec runSpec =
    Opt.objectMaySkip
        [ Opt.field JE.string
            ( "interpreter", runSpecInterpreterToString runSpec.interpreter )
        , Opt.field JE.string ( "file", runSpec.file )
        , Opt.field JE.string ( "distribution", runSpec.distribution )
        , Opt.field JE.string ( "release", runSpec.release )
        , Opt.field JE.string ( "version", runSpec.version )
        , Opt.optionalField JE.string
            ( "restartableEntryPoints"
            , stringToMaybe runSpec.restartableEntryPoints
            )
        , Opt.optionalField (JE.list encodeExecDepends)
            ( "execDepends", listToMaybe runSpec.execDepends )
        , Opt.optionalField
            (JE.dict identity encodeTimeoutPolicyTime)
            ( "timeoutPolicy", dictToMaybe runSpec.timeoutPolicy )
        , Opt.optionalField
            (JE.dict identity encodeSystemRequirements)
            ( "systemRequirements", dictToMaybe runSpec.systemRequirements )
        , Opt.optionalField (JE.list encodeAssetDepends)
            ( "assetDepends", listToMaybe runSpec.assetDepends )
        ]


defaultValueToMaybe default =
    case default of
        DefaultValString val ->
            if String.isEmpty val then
                Nothing

            else
                Just default

        _ ->
            Just default


encodeDefault default =
    case default of
        DefaultValInt val ->
            JE.int val

        DefaultValFloat val ->
            JE.float val

        DefaultValBool val ->
            JE.bool val

        DefaultValString val ->
            JE.string val


encodeInputSpec inputSpec =
    Opt.objectMaySkip
        [ Opt.field JE.string ( "name", inputSpec.name )
        , Opt.field JE.string ( "label", inputSpec.label )
        , Opt.field JE.string
            ( "class"
            , inputSpecClassToString inputSpec.class
            )
        , Opt.field JE.bool ( "optional", inputSpec.optional )
        , Opt.optionalField encodeDefault
            ( "default", defaultValueToMaybe inputSpec.default )
        , Opt.optionalField (JE.list JE.string)
            ( "patterns", listToMaybe inputSpec.patterns )
        , Opt.optionalField (JE.list JE.string)
            ( "choices", listToMaybe inputSpec.choices )
        , Opt.optionalField JE.string ( "type", inputSpec.type_ )
        , Opt.field JE.string ( "help", inputSpec.help )
        , Opt.field JE.string ( "group", inputSpec.group )
        , Opt.optionalField (JE.list encodeInputSpecSuggestion)
            ( "suggestions", listToMaybe inputSpec.suggestions )
        ]


encodeInputSpecSuggestion suggestion =
    Opt.objectMaySkip
        [ Opt.field JE.string ( "name", suggestion.name )
        , Opt.field JE.string ( "project", suggestion.project )
        , Opt.field JE.string ( "path", suggestion.path )
        , Opt.optionalField JE.string
            ( "region", stringToMaybe suggestion.region )
        ]
