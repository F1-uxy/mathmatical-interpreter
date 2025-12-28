namespace MathInterpreter.Exceptions

open System
open System.Runtime.Serialization

[<Serializable>]
type GenerationException =
    inherit Exception

    new (message: string) =
        { inherit Exception(message) }

    new (message: string, inner: Exception) =
        { inherit Exception(message, inner) }

    new (info: SerializationInfo, context: StreamingContext) =
        { inherit Exception(info, context) }