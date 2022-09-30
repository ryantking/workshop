// Code generated by easyjson for marshaling/unmarshaling. DO NOT EDIT.

package yabai

import (
	json "encoding/json"

	easyjson "github.com/mailru/easyjson"
	jlexer "github.com/mailru/easyjson/jlexer"
	jwriter "github.com/mailru/easyjson/jwriter"
)

// suppress unused package warning
var (
	_ *json.RawMessage
	_ *jlexer.Lexer
	_ *jwriter.Writer
	_ easyjson.Marshaler
)

func easyjsonEeb5ff34DecodeGithubComRyantkingYabaictlYabai(in *jlexer.Lexer, out *Space) {
	isTopLevel := in.IsStart()
	if in.IsNull() {
		if isTopLevel {
			in.Consumed()
		}
		in.Skip()
		return
	}
	in.Delim('{')
	for !in.IsDelim('}') {
		key := in.UnsafeFieldName(false)
		in.WantColon()
		if in.IsNull() {
			in.Skip()
			in.WantComma()
			continue
		}
		switch key {
		case "index":
			out.Index = int(in.Int())
		case "label":
			out.Label = string(in.String())
		case "display":
			out.Display = int(in.Int())
		case "windows":
			if in.IsNull() {
				in.Skip()
				out.Windows = nil
			} else {
				in.Delim('[')
				if out.Windows == nil {
					if !in.IsDelim(']') {
						out.Windows = make([]int, 0, 8)
					} else {
						out.Windows = []int{}
					}
				} else {
					out.Windows = (out.Windows)[:0]
				}
				for !in.IsDelim(']') {
					var v1 int
					v1 = int(in.Int())
					out.Windows = append(out.Windows, v1)
					in.WantComma()
				}
				in.Delim(']')
			}
		case "first-window":
			out.FirstWindow = int(in.Int())
		case "last-window":
			out.LastWindow = int(in.Int())
		case "has-focus":
			out.HasFocus = bool(in.Bool())
		case "is-visible":
			out.IsVisible = bool(in.Bool())
		case "is-native-fullscreen":
			out.IsNativeFullScreen = bool(in.Bool())
		default:
			in.SkipRecursive()
		}
		in.WantComma()
	}
	in.Delim('}')
	if isTopLevel {
		in.Consumed()
	}
}

func easyjsonEeb5ff34EncodeGithubComRyantkingYabaictlYabai(out *jwriter.Writer, in Space) {
	out.RawByte('{')
	first := true
	_ = first
	{
		const prefix string = ",\"index\":"
		out.RawString(prefix[1:])
		out.Int(int(in.Index))
	}
	{
		const prefix string = ",\"label\":"
		out.RawString(prefix)
		out.String(string(in.Label))
	}
	{
		const prefix string = ",\"display\":"
		out.RawString(prefix)
		out.Int(int(in.Display))
	}
	{
		const prefix string = ",\"windows\":"
		out.RawString(prefix)
		if in.Windows == nil && (out.Flags&jwriter.NilSliceAsEmpty) == 0 {
			out.RawString("null")
		} else {
			out.RawByte('[')
			for v2, v3 := range in.Windows {
				if v2 > 0 {
					out.RawByte(',')
				}
				out.Int(int(v3))
			}
			out.RawByte(']')
		}
	}
	{
		const prefix string = ",\"first-window\":"
		out.RawString(prefix)
		out.Int(int(in.FirstWindow))
	}
	{
		const prefix string = ",\"last-window\":"
		out.RawString(prefix)
		out.Int(int(in.LastWindow))
	}
	{
		const prefix string = ",\"has-focus\":"
		out.RawString(prefix)
		out.Bool(bool(in.HasFocus))
	}
	{
		const prefix string = ",\"is-visible\":"
		out.RawString(prefix)
		out.Bool(bool(in.IsVisible))
	}
	{
		const prefix string = ",\"is-native-fullscreen\":"
		out.RawString(prefix)
		out.Bool(bool(in.IsNativeFullScreen))
	}
	out.RawByte('}')
}

// MarshalJSON supports json.Marshaler interface
func (v Space) MarshalJSON() ([]byte, error) {
	w := jwriter.Writer{}
	easyjsonEeb5ff34EncodeGithubComRyantkingYabaictlYabai(&w, v)
	return w.Buffer.BuildBytes(), w.Error
}

// MarshalEasyJSON supports easyjson.Marshaler interface
func (v Space) MarshalEasyJSON(w *jwriter.Writer) {
	easyjsonEeb5ff34EncodeGithubComRyantkingYabaictlYabai(w, v)
}

// UnmarshalJSON supports json.Unmarshaler interface
func (v *Space) UnmarshalJSON(data []byte) error {
	r := jlexer.Lexer{Data: data}
	easyjsonEeb5ff34DecodeGithubComRyantkingYabaictlYabai(&r, v)
	return r.Error()
}

// UnmarshalEasyJSON supports easyjson.Unmarshaler interface
func (v *Space) UnmarshalEasyJSON(l *jlexer.Lexer) {
	easyjsonEeb5ff34DecodeGithubComRyantkingYabaictlYabai(l, v)
}
