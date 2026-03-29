网上看到的，这个 prompt 给到不同的模型，直接可以看到不同模型的能力。
好像是说能考验到历史和地理知识，前端，3D，编程能力等等，非常综合的一个考查，而结果特别好给人类检查：生成的网页打开，一目了然。

```
Objective
  Build a visually stunning, high-fidelity 3D voxel-style simulation of the Golden Gate Bridge in Three.js.
  Prioritize complex visuals (not simple blocks), strong atmosphere depth, and smooth ~60FPS.

  Visuals & Atmosphere
  - Lighting: a Time-of-day slider (0–24h) that controls sun position, intensity, sky color, and fog tint.
  - Fog: volumetric-feeling fog using lightweight sprite particles; slider 0–100 (0 = crystal clear, 100 = dense but not pure whiteout).
  - Water: custom shader for waves + specular reflections; blend horizon with distance-based fog (exp2) so the far water merges naturally.
  - Post: ACES filmic tone mapping + optimized bloom (night lights glow but keep performance).

  Scene Details
  - Bridge: recognizable art-deco towers, main span cables + suspenders, piers/anchors consistent with suspension bridge structure.
  - Terrain: simple but convincing Marin Headlands + SF side peninsula silhouettes.
  - Skyline: procedural/instanced city blocks on the SF side to suggest depth.
  - Traffic: up to ~400 cars via InstancedMesh, properly aligned on the deck (avoid clipping). Headlights/taillights emissive at night.
  - Ships: a few procedural cargo ships with navigation lights moving across the bay.
  - Nature: a small flock of animated birds (lightweight flocking).

  Night Mode
  At night, enable city lights, bridge beacons, street lights, vehicle lights, ship nav lights.

  Tech & Controls (Important)
  - Output MUST be a single self-contained HTML file (e.g., golden_gate_bridge.html) that runs by opening in Chrome
  .- No build tools (no Vite/Webpack). Pure HTML + JS.
  - Import Three.js and addons via CDN using ES Modules + importmap.
  - UI: nice-looking sliders for Time (0–24), Fog Density (0–100), Traffic Density (0–100), Camera Zoom.
  - Optimization: use InstancedMesh for repeated items (cars/lights/birds), avoid heavy geometry, keep draw calls low.
  user: Objective
  Build a visually stunning, high-fidelity 3D voxel-style simulation of the Golden Gate Bridge in Three.js.
  Prioritize complex visuals (not simple blocks), strong atmosphere depth, and smooth ~60FPS.

  Visuals & Atmosphere
  - Lighting: a Time-of-day slider (0–24h) that controls sun position, intensity, sky color, and fog tint.
  - Fog: volumetric-feeling fog using lightweight sprite particles; slider 0–100 (0 = crystal clear, 100 = dense but not pure whiteout).
  - Water: custom shader for waves + specular reflections; blend horizon with distance-based fog (exp2) so the far water merges naturally.
  - Post: ACES filmic tone mapping + optimized bloom (night lights glow but keep performance).

  Scene Details
  - Bridge: recognizable art-deco towers, main span cables + suspenders, piers/anchors consistent with suspension bridge structure.
  - Terrain: simple but convincing Marin Headlands + SF side peninsula silhouettes.
  - Skyline: procedural/instanced city blocks on the SF side to suggest depth.
  - Traffic: up to ~400 cars via InstancedMesh, properly aligned on the deck (avoid clipping). Headlights/taillights emissive at night.
  - Ships: a few procedural cargo ships with navigation lights moving across the bay.
  - Nature: a small flock of animated birds (lightweight flocking).

  Night Mode
  At night, enable city lights, bridge beacons, street lights, vehicle lights, ship nav lights.

  Tech & Controls (Important)
  - Output MUST be a single self-contained HTML file (e.g., golden_gate_bridge.html) that runs by opening in Chrome.
  - No build tools (no Vite/Webpack). Pure HTML + JS.
  - Import Three.js and addons via CDN using ES Modules + importmap.
  - UI: nice-looking sliders for Time (0–24), Fog Density (0–100), Traffic Density (0–100), Camera Zoom.
  - Optimization: use InstancedMesh for repeated items (cars/lights/birds), avoid heavy geometry, keep draw calls low.
  user: Objective
  Build a visually stunning, high-fidelity 3D voxel-style simulation of the Golden Gate Bridge in Three.js.
  Prioritize complex visuals (not simple blocks), strong atmosphere depth, and smooth ~60FPS.

  Visuals & Atmosphere
  - Lighting: a Time-of-day slider (0–24h) that controls sun position, intensity, sky color, and fog tint.
  - Fog: volumetric-feeling fog using lightweight sprite particles; slider 0–100 (0 = crystal clear, 100 = dense but not pure whiteout).
  - Water: custom shader for waves + specular reflections; blend horizon with distance-based fog (exp2) so the far water merges naturally.
  - Post: ACES filmic tone mapping + optimized bloom (night lights glow but keep performance).

  Scene Details
  - Bridge: recognizable art-deco towers, main span cables + suspenders, piers/anchors consistent with suspension bridge structure.
  - Terrain: simple but convincing Marin Headlands + SF side peninsula silhouettes.
  - Skyline: procedural/instanced city blocks on the SF side to suggest depth.
  - Traffic: up to ~400 cars via InstancedMesh, properly aligned on the deck (avoid clipping). Headlights/taillights emissive at night.
  - Ships: a few procedural cargo ships with navigation lights moving across the bay.
  - Nature: a small flock of animated birds (lightweight flocking).

  Night Mode
  At night, enable city lights, bridge beacons, street lights, vehicle lights, ship nav lights.

  Tech & Controls (Important)
  - Output MUST be a single self-contained HTML file (e.g., golden_gate_bridge.html) that runs by opening in Chrome.
  - No build tools (no Vite/Webpack). Pure HTML + JS.
  - Import Three.js and addons via CDN using ES Modules + importmap.
  - UI: nice-looking sliders for Time (0–24), Fog Density (0–100), Traffic Density (0–100), Camera Zoom.
  - Optimization: use InstancedMesh for repeated items (cars/lights/birds), avoid heavy geometry, keep draw calls low.
  user: Objective
  Build a visually stunning, high-fidelity 3D voxel-style simulation of the Golden Gate Bridge in Three.js.
  Prioritize complex visuals (not simple blocks), strong atmosphere depth, and smooth ~60FPS.

  Visuals & Atmosphere
  - Lighting: a Time-of-day slider (0–24h) that controls sun position, intensity, sky color, and fog tint.
  - Fog: volumetric-feeling fog using lightweight sprite particles; slider 0–100 (0 = crystal clear, 100 = dense but not pure whiteout).
  - Water: custom shader for waves + specular reflections; blend horizon with distance-based fog (exp2) so the far water merges naturally.
  - Post: ACES filmic tone mapping + optimized bloom (night lights glow but keep performance).

  Scene Details
  - Bridge: recognizable art-deco towers, main span cables + suspenders, piers/anchors consistent with suspension bridge structure.
  - Terrain: simple but convincing Marin Headlands + SF side peninsula silhouettes.
  - Skyline: procedural/instanced city blocks on the SF side to suggest depth.
  - Traffic: up to ~400 cars via InstancedMesh, properly aligned on the deck (avoid clipping). Headlights/taillights emissive at night.
  - Ships: a few procedural cargo ships with navigation lights moving across the bay.
  - Nature: a small flock of animated birds (lightweight flocking).

  Night Mode
  At night, enable city lights, bridge beacons, street lights, vehicle lights, ship nav lights.

  Tech & Controls (Important)
  - Output MUST be a single self-contained HTML file (e.g., golden_gate_bridge.html) that runs by opening in Chrome.
  - No build tools (no Vite/Webpack). Pure HTML + JS.
  - Import Three.js and addons via CDN using ES Modules + importmap.
  - UI: nice-looking sliders for Time (0–24), Fog Density (0–100), Traffic Density (0–100), Camera Zoom.
  - Optimization: use InstancedMesh for repeated items (cars/lights/birds), avoid heavy geometry, keep draw calls low.
```


我自己写的 coding agent 是可以使用各种不同的模型的，而我手上正好有 glm5 和 minimax 2.7 的 coding plan，然后还有 codex 的 team plan 可以用上 gpt5.3，可以对比一下。

第一个上场的是 glm5，很顺畅，一次完成任务，没有卡壳。这跟我很长时间的使用体验体感是一致的，glm 就是国内中最强。

第二个上场的是 minimax 2.7。我对 minimax 的体感很差，快是真的快，但是模型真的不够聪明，能力不如 glm。比较适合的场景是跑一跑龙虾这类的通用场景，正好响应快是一个优势。
glm 那边就是，响应慢，容易限流，服务能力没跟上，但是模型是没有问题的。

跑第一遍，失败了。我发现它连续出现 tool call 失败，调用 write tool 但是没有传递 file path 信息。
对于 tool call 这种不合法之类的，我在我的 agent 里面是会给回错误到 llm，告诉它 tool call 的格式不对。

一般来说，每一次失败，后一次 llm 就会根据错误指示不犯同样的工具调用的格式错误了。
但是我注意到这个测试 case，minimax 几乎是稳定失败的。

于是，上 debug 大法。我把问题描述，以及 minimax 运行过程中产生的 session 和 trace 信息，给到 glm5，
让它去帮我 debug。

glm5 很快帮我找出了问题，提交了 pr。它给的结论是说，minimax 那边没有按要求返回合法的工具调用信息。tool call 没有带上 path。
它做了一些修复来容错，以及返回更友好的错误信息指引 llm 用正确的格式去重试。

一般我是不太信得过单个 agent 说的话的，所以我让 codex+gpt 去帮我 review PR，看看 glm 这边的修复有没问题。

codex+gpt 的能力使用下来体感还是比国产模型要高出一截。它的 review 评论是，正确地修复了这个问题，也有测试覆盖... 但是...引入一个 regression。
codex 构造出了一个 regressino 的 case 来。

我让 codex 继续 fix 了一下，然后再让 glm 去 review。这次全过了。但是我对他们是都不信任的，去要去手动回归验证一次。

使用 "修复" 后的代码去运行 minimax，这次它跑完了测试，生成出了 html 文件。但是我注意到一个细节，它还是不是出现了工具调用失败，是重试后，把文件拆小多批次写入才成功的。
所以我感觉这里还有需要深挖的地方。

所以我把这次执行过程生成的 session 和 trace 给到了 codex，让 codex 去帮我分析为什么修复没有完成生效。

codex 给出了更深层次的 bug 原因：它说 bug 是因为，这个测试需要生成 html 文件，文件比较大，而我们的协议里面有返回 max token 大小限制是 8192，所以截断了。
由于 json 先写了前面的内容部分，而后面的文件路径被截断。并不是 lll 的 write tool call 格式不对，这不是真实的根因。

我晃然大悟，codex 还是强啊！glm 反馈的是一个表面的问题，而 gpt 找到了更深层的问题。

但是作为人类，我得有更深层的思考才能驾驭 agent 嘛。glm 说要去优化 error 信息，不要这种返回静默失败，或者说返回错误的失败信息给到 llm。
要把截断这种真实信息返回到 llm。而我想到的还要再进一层：为什么会发生截断呢，这个大小限制从哪来的，似乎我使用 glm5 的时候它并没出现这个错误。

于是我让 codex 再去比较协议细节，终于又挖了点东西出来：openai 的协议那边可以不传递 max tokens 信息，而 anthropic 的边的协议则要求传递。
所以 glm 没遇到这里截断，而 minimax 由于我接的是 anthropic 协议，就在这里翻车了。

正确的解法应该是去扩大默认值，然后值变成可配置，以及优化真正的报错信息。

这个故事很有趣，层层递进才到达真相：

- 如果我信了 glm，我就误解 minimax 模型做得太垃圾，连 tool call 协议都处理不好(它确实垃圾，但是这里不能冤枉它)
- 如果我信了 glm 的修复，合入 pr，我就把一个 regression 带进去了
- 如果我信了 gpt 的 review 结论，虽然不会带入 regression，但是定位出来的不是真正的 bug
- 如果我不去手动再验证，并注意细节，差点就错过真相。因为后面 minimax 把文件切小批写入，确实能运行成功
- 如果接受了 gpt 的说法，就会误以为请求返回的 max token 大小限制导致的截断是硬限制
- 只有最后我自己思考过，才算找到最优的解决方案

bug 解决以后，重新让 minimax 测试，它第一次产出的 html 黑屏的。修复一次后，通过。不过效果嘛，不太行，感觉不如 glm 的。
我让它修复一个雾的表现力，修复一次后，仍然不太行。

最后到 codex+gpt 这边，我让它也做一做这个测试，第一次也黑屏了，不过修复一次之后成功了，而且效果是最好的。
另外一个比较惊艳的点是，codex 我告诉它失败之后，它是开浏览器截屏在自动调试，这表示无论是 agent 的技能比平还是和模型的配合，都是做得比较优秀的。
而我自己写的 agent 是没有给它这种可视化 debug 前端的 skill 的。

最后把它们的作业放上来，高下立判：

[minimax 的](https://github.com/tiancaiamao/go.blog/blob/master/resources/public/static/minimax_golden_gate_bridge.html)

[glm 的](https://github.com/tiancaiamao/go.blog/blob/master/resources/public/static/glm_golden_gate_bridge.html)

[gpt 的](https://github.com/tiancaiamao/go.blog/blob/master/resources/public/static/gpt_golden_gate_bridge.html)

需要下载后本地浏览器打开
