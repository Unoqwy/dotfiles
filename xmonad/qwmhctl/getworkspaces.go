package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/jezek/xgb/xproto"
	"github.com/jezek/xgbutil"
	"github.com/jezek/xgbutil/xevent"
	"github.com/jezek/xgbutil/xprop"
	"github.com/jezek/xgbutil/xwindow"
)

const (
	AtomDesktopNames   = "_QDE_DESKTOP_NAMES"
	AtomDesktopBins    = "_QDE_DESKTOP_BINS"
	AtomCurrentDesktop = "_QDE_CURRENT_DESKTOP"
)

const (
	Visible byte = 1 << iota
	Empty
	HasMinimized
	HasFullscreen
)

type Workspace struct {
	Id            int    `json:"id"`
	Name          string `json:"name"`
	Pinned        bool   `json:"pinned"`
	Focused       bool   `json:"focused"`
	Windows       int    `json:"windows"`
	Minimized     int    `json:"minimized"`
	HasFullscreen bool   `json:"hasfullscreen"`
}

func CmdGetWorkspaces(listen bool) error {
	xu, err := xgbutil.NewConn()
	if err != nil {
		return err
	}
	execGetWorkspaces(xu)
	if listen {
		xwindow.New(xu, xu.RootWin()).Listen(xproto.EventMaskPropertyChange)
		xevent.PropertyNotifyFun(handlePropertyChange).Connect(xu, xu.RootWin())
		xevent.Main(xu)
	}
	return nil
}

func handlePropertyChange(xu *xgbutil.XUtil, ev xevent.PropertyNotifyEvent) {
	atmName, err := xprop.AtomName(xu, ev.Atom)
	if err != nil {
		return
	}
	if atmName == AtomCurrentDesktop {
		execGetWorkspaces(xu)
	}
}

func execGetWorkspaces(xu *xgbutil.XUtil) error {
	root := xu.RootWin()
	current, err := xprop.PropValNum(xprop.GetProperty(xu, root, AtomCurrentDesktop))
	fmt.Println(current)
	desktopNames, err := xprop.PropValStrs(xprop.GetProperty(xu, root, AtomDesktopNames))
	if err != nil {
		return err
	}
	binsReply, err := xprop.GetProperty(xu, root, AtomDesktopBins)
	if err != nil {
		return err
	}
	bins := binsReply.Value
	workspaces := make([]Workspace, 0)
	for idx, bin := range bins {
		name := desktopNames[idx]
		workspace := Workspace{
			Id:      idx,
			Name:    name,
			Focused: current == uint(idx),
		}
		if bin&Visible != 0 {
			workspace.Pinned = true
		}
		if bin&Empty != 0 {
			workspace.Windows = 0
		} else {
			workspace.Windows = 1
		}
		if bin&HasMinimized != 0 {
			workspace.Minimized = 1
		}
		if bin&HasFullscreen != 0 {
			workspace.HasFullscreen = true
		}
		if !workspace.Focused && !workspace.Pinned {
			continue
		}
		workspaces = append(workspaces, workspace)
	}
	enc := json.NewEncoder(os.Stdout)
	return enc.Encode(workspaces)
}
