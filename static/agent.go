package player

import (
	"io"
	"log"
	"net"
	// "runtime"
	"encoding/binary"
	"github.com/tiancaiamao/ouster/data"
)

type PlayerClass uint8
const (
	_ = iota
	BRUTE = iota
)
// mostly the same as data.Player, but this is in memory instead.
type Player struct {
	id uint32 // alloc by scene
	name string
	class PlayerClass
	hp int
	mp int

	carried []int

	conn net.Conn
	client chan interface{}
	aoi chan interface{}
	scene chan interface{}
}

func NewPlayer(playerId uint32, playerData *data.Player, conn net.Conn, scene chan interface{}) *Player {
	return &Player {
		id: playerId,
		name: playerData.Name,
		class: PlayerClass(playerData.Class),
		hp: playerData.HP,
		mp: playerData.MP,
		carried: playerData.Carried,
		conn: conn,
	}
}

func (this *Player) loop() {	
	for {
		select {
		case _, ok := (<-this.client):
			if ok {
				// 解析packet，决定自己处理或者向其它地方转发
			} else {
			}
		case <-this.scene:
		case <-this.aoi:
			// 来自aoi的消息
		}
	}
}

func (player *Player) Go() {
	var header [4]byte
	go player.loop()
	for {
		n, err := io.ReadFull(player.conn, header[:])
		if n == 0 && err == io.EOF {

			// 处理出错
			break
		} else if err != nil {
			log.Println("error receiving header:", err)
			break
		}

		// data
		size := binary.BigEndian.Uint16(header[:])
		data := make([]byte, size)
		n, err = io.ReadFull(player.conn, data)

		if err != nil {
			log.Println("error receiving msg:", err)
			break
		}
		player.client <- data
	}
}
