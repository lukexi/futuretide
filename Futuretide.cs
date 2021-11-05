using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Futuretide : MonoBehaviour {
    public OSC osc;
    public Transform prefab;
    public int instances = 1024;
    public float radius = 50;
    public Dictionary<string, object>[] events;
    public int write_head = 0;

    public double last_time_update;
    public double cycles;
    public double cps;
    // Start is called before the first frame update
    void Start() {

        osc.SetAddressHandler("/dirt/play", OnReceivePlay);
        osc.SetAddressHandler("/time", OnReceiveTime);

        for (int i = 0; i < instances; i++) {
            Transform t = Instantiate(prefab);
            // t.localPosition = Random.insideUnitSphere * radius;
            t.SetParent(transform);
        }

        events = new Dictionary<string, object>[1024];
    }

    // Update is called once per frame
    void Update() {
        int i = 0;
        foreach (Transform child in transform) {
            // child.position += Vector3.up * 1.0f;
            if (i < write_head) {
                child.position = new Vector3(0,0,4f * (float)((float)events[i]["cycle"] - cycles));
            }
            i++;
        }
    }

    void OnReceiveTime(OscMessage message) {
        // Debug.Log(message);
        cycles = message.GetDouble(0);
        cps = message.GetDouble(1);
    }

    void OnReceivePlay(OscMessage message) {

        var ev = new Dictionary<string,object>();

        for (int i = 2; i < (message.values.Count - 2) / 2; i++) {
            var key = message.GetString(i*2 + 0);
            var val = message.values[i*2 + 1];
            ev.Add(key, val);
        }

        foreach(KeyValuePair<string, object> kv in ev) {
            // Debug.Log(kv.Key + " = ");
            // Debug.Log(kv.Key + " = " + kv.Value.ToString());
        }
        events[write_head] = ev;
        write_head = (write_head + 1) % 1024;
    }
}
