// Modified (cut down / simplified) version of OculusRoomTiny sample from Oculus 0.7 SDK
// Modified by: Matt Newport (matt@mattnewport.com)
// Original copyright / license info below:

/************************************************************************************
Content     :   First-person view test application for Oculus Rift
Created     :   11th May 2015
Authors     :   Tom Heath
Copyright   :   Copyright 2015 Oculus, Inc. All Rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*************************************************************************************/
/// This is an entry-level sample, showing a minimal VR sample, in a simple environment. Use WASD
/// keys to move around, and cursor keys. Dismiss the health and safety warning by tapping the
/// headset, or pressing any key. It runs with DirectX11.

#include <algorithm>
#include <array>
#include <functional>
#include <initializer_list>
#include <memory>
#include <type_traits>
#include <vector>

#include <comdef.h>
#include <comip.h>
#include <d3d11.h>
#include <d3dcompiler.h>
#include <DirectXMath.h>

#include <OVR_CAPI_D3D.h>

#pragma comment(lib, "d3d11.lib")
#pragma comment(lib, "d3dcompiler.lib")
#pragma comment(lib, "dxgi.lib")

using namespace DirectX;
using namespace std;

#ifndef VALIDATE
#define VALIDATE(x, msg)                                                  \
    if (!(x)) {                                                           \
        MessageBoxA(NULL, (msg), "OculusRoomTiny", MB_ICONERROR | MB_OK); \
        exit(-1);                                                         \
    }
#endif

// Define _com_ptr_t COM smart pointer typedefs for all the D3D and DXGI interfaces we use
#define COM_SMARTPTR_TYPEDEF(x) _COM_SMARTPTR_TYPEDEF(x, __uuidof(x))
COM_SMARTPTR_TYPEDEF(ID3D11BlendState);
COM_SMARTPTR_TYPEDEF(ID3D11Buffer);
COM_SMARTPTR_TYPEDEF(ID3D11DepthStencilState);
COM_SMARTPTR_TYPEDEF(ID3D11DepthStencilView);
COM_SMARTPTR_TYPEDEF(ID3D11Device);
COM_SMARTPTR_TYPEDEF(ID3D11DeviceContext);
COM_SMARTPTR_TYPEDEF(ID3D11InputLayout);
COM_SMARTPTR_TYPEDEF(ID3D11PixelShader);
COM_SMARTPTR_TYPEDEF(ID3D11RasterizerState);
COM_SMARTPTR_TYPEDEF(ID3D11RenderTargetView);
COM_SMARTPTR_TYPEDEF(ID3D11SamplerState);
COM_SMARTPTR_TYPEDEF(ID3D11ShaderResourceView);
COM_SMARTPTR_TYPEDEF(ID3D11Texture2D);
COM_SMARTPTR_TYPEDEF(ID3D11VertexShader);
COM_SMARTPTR_TYPEDEF(ID3DBlob);
COM_SMARTPTR_TYPEDEF(IDXGIAdapter);
COM_SMARTPTR_TYPEDEF(IDXGIDevice1);
COM_SMARTPTR_TYPEDEF(IDXGIFactory);
COM_SMARTPTR_TYPEDEF(IDXGISwapChain);

struct Window {
    HWND Hwnd = nullptr;
    bool Running = false;
    bool Keys[256] = {};

    static LRESULT CALLBACK WindowProc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam) {
        auto p = reinterpret_cast<Window*>(GetWindowLongPtr(hWnd, 0));
        switch (Msg) {
            case WM_KEYDOWN:
                p->Keys[wParam] = true;
                break;
            case WM_KEYUP:
                p->Keys[wParam] = false;
                break;
            case WM_DESTROY:
                p->Running = false;
                break;
            default:
                return DefWindowProcW(hWnd, Msg, wParam, lParam);
        }
        if ((p->Keys['Q'] && p->Keys[VK_CONTROL]) || p->Keys[VK_ESCAPE]) {
            p->Running = false;
        }
        return 0;
    }

    Window(HINSTANCE hinst, LPCWSTR title) : Running{true} {
        WNDCLASSW wc{};
        wc.lpszClassName = L"App";
        wc.style = CS_OWNDC;
        wc.lpfnWndProc = WindowProc;
        wc.cbWndExtra = sizeof(this);
        RegisterClassW(&wc);

        // adjust the window size and show at InitDevice time
        Hwnd =
            CreateWindowW(wc.lpszClassName, title, WS_OVERLAPPEDWINDOW, 0, 0, 0, 0, 0, 0, hinst, 0);

        SetWindowLongPtr(Hwnd, 0, reinterpret_cast<LONG_PTR>(this));
    }

    auto HandleMessages() const {
        MSG msg{};
        while (PeekMessage(&msg, nullptr, 0U, 0U, PM_REMOVE)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        return Running;
    }

    void Run(ovrResult (*MainLoop)(const Window& window)) const {
        auto tryReinit = false;
        while (HandleMessages()) {
            auto res = MainLoop(*this);
            // Try and reinit if display lost, otherwise this is a hard error
            tryReinit = res == ovrError_DisplayLost ? true : tryReinit;
            if (!tryReinit) {
                ovrErrorInfo errorInfo{};
                ovr_GetLastErrorInfo(&errorInfo);
                VALIDATE(OVR_SUCCESS(res), errorInfo.ErrorString);
                break;
            }
            // Sleep a bit before retrying to reduce CPU load while the HMD is disconnected
            Sleep(10);
        }
    }
};

struct DepthBuffer {
    ID3D11DepthStencilViewPtr TexDsv;

    DepthBuffer(ID3D11Device* Device, ovrSizei size) {
        ID3D11Texture2DPtr Tex;
        Device->CreateTexture2D(
            data({CD3D11_TEXTURE2D_DESC(DXGI_FORMAT_D24_UNORM_S8_UINT, size.w, size.h, 1, 1,
                                        D3D11_BIND_DEPTH_STENCIL)}),
            nullptr, &Tex);
        Device->CreateDepthStencilView(Tex, nullptr, &TexDsv);
    }
};

struct DirectX11 {
    int WinSizeW = 0, WinSizeH = 0;
    ID3D11DevicePtr Device;
    ID3D11DeviceContextPtr Context;
    IDXGISwapChainPtr SwapChain;
    ID3D11Texture2DPtr BackBuffer;
    ID3D11VertexShaderPtr D3DVert;
    ID3D11PixelShaderPtr D3DPix;
    ID3D11InputLayoutPtr InputLayout;
    ID3D11SamplerStatePtr SamplerState;
    ID3D11BufferPtr ConstantBuffer;

    DirectX11(HWND window, int vpW, int vpH, const LUID* pLuid);

    void SetAndClearRenderTarget(ID3D11RenderTargetView* rendertarget,
                                 DepthBuffer* depthbuffer) const {
        Context->OMSetRenderTargets(1, &rendertarget, depthbuffer->TexDsv);
        Context->ClearRenderTargetView(rendertarget, data({0.0f, 0.0f, 0.0f, 0.0f}));
        Context->ClearDepthStencilView(depthbuffer->TexDsv, D3D11_CLEAR_DEPTH | D3D11_CLEAR_STENCIL,
                                       1.0f, 0);
    }

    void SetViewport(const ovrRecti& vp) const {
        Context->RSSetViewports(
            1, data({D3D11_VIEWPORT{float(vp.Pos.x), float(vp.Pos.y), float(vp.Size.w),
                                    float(vp.Size.h), 0.0f, 1.0f}}));
    }
};

enum class TextureFill { AUTO_WHITE, AUTO_WALL, AUTO_FLOOR, AUTO_CEILING };

auto createTexture(ID3D11Device* device, ID3D11DeviceContext* context, TextureFill texFill) {
    constexpr auto widthHeight = 256;
    auto texDesc = CD3D11_TEXTURE2D_DESC(DXGI_FORMAT_R8G8B8A8_UNORM, widthHeight, widthHeight, 1, 0,
                                         D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET);
    texDesc.MiscFlags = D3D11_RESOURCE_MISC_GENERATE_MIPS;

    ID3D11Texture2DPtr tex;
    device->CreateTexture2D(&texDesc, nullptr, &tex);
    ID3D11ShaderResourceViewPtr texSrv;
    device->CreateShaderResourceView(tex, nullptr, &texSrv);

    // Fill texture with requested pattern
    enum : uint32_t {
        DARK_GREY = 0xff3c3c3c,
        MID_GREY = 0xff505050,
        LIGHT_GREY = 0xffb4b4b4,
        WHITE = 0xffffffff
    };
    uint32_t pix[widthHeight * widthHeight] = {};
    for (auto y = 0u; y < widthHeight; ++y)
        for (auto x = 0u; x < widthHeight; ++x) {
            auto& curr = pix[y * widthHeight + x];
            switch (texFill) {
                case (TextureFill::AUTO_WALL): {
                    const bool a = y / 4 % 16 == 0, b = x / 4 % 16 == 0;
                    const bool c = x / 4 % 32 != 0, d = y / 64 % 2 == 0;
                    curr = a || b && c ^ d ? DARK_GREY : LIGHT_GREY;
                } break;
                case (TextureFill::AUTO_FLOOR):
                    curr = x / 128 ^ y / 128 ? LIGHT_GREY : MID_GREY;
                    break;
                case (TextureFill::AUTO_CEILING):
                    curr = x / 4 == 0 || y / 4 == 0 ? MID_GREY : LIGHT_GREY;
                    break;
                case (TextureFill::AUTO_WHITE):
                default:
                    curr = WHITE;
                    break;
            }
        }
    context->UpdateSubresource(tex, 0, nullptr, data(pix), widthHeight * sizeof(pix[0]), 0);
    context->GenerateMips(texSrv);

    return texSrv;
}

struct Vertex {
    XMFLOAT3 pos;
    uint32_t c;
    XMFLOAT2 uv;
};

struct TriangleSet {
    vector<Vertex> Vertices;
    vector<uint16_t> Indices;

    struct Box {
        float x1, y1, z1, x2, y2, z2;
        uint32_t c;
    };

    TriangleSet(initializer_list<Box> boxes) {
        for (const auto& b : boxes) AddBox(b);
    }

    void AddBox(const Box& b) {
        XMFLOAT3 ps[1 << 3];
        for (int i = 0; i < int(std::size(ps)); ++i) {
            ps[i] = XMFLOAT3{i & (1 << 0) ? b.x1 : b.x2, i & (1 << 1) ? b.y1 : b.y2,
                             i & (1 << 2) ? b.z1 : b.z2};
        }
        uint16_t qis[6] = {0, 1, 2, 2, 1, 3};

        const auto selectVertex = [](int face, int vertex) {
            auto rotr3 = [](int x, int rot) { return x >> rot | (x << (3 - rot)) & 7; };
            const auto fixed = face % 3;
            const auto offset = (1 << fixed) * (face / 3);
            return (rotr3(vertex, 2 - fixed) + offset) & 7;
        };

        const auto modifyColor = [](uint32_t c, XMFLOAT3 pos) {
            auto length = [](const auto& v) { return XMVectorGetX(XMVector3Length(v)); };
            const auto v = XMLoadFloat3(&pos);
            const auto dist1 = length(v + XMVectorSet(2, -4, 2, 0)),
                       dist2 = length(v + XMVectorSet(-3, -4, 3, 0)),
                       dist3 = length(v + XMVectorSet(4, -3, -25, 0));
            const auto mod =
                (rand() % 160 + 192 * (.65f + 8 / dist1 + 1 / dist2 + 4 / dist3)) / 255.0f;
            auto saturate = [](float x) { return x > 255 ? 255u : uint32_t(x); };
            const auto r = saturate(((c >> 16) & 0xff) * mod),
                       g = saturate(((c >> 8) & 0xff) * mod), b = saturate(((c >> 0) & 0xff) * mod);
            return (c & 0xff000000) + (r << 16) + (g << 8) + b;
        };

        Vertex vs[24];
        uint16_t is[36];
        for (int face = 0; face < 6; ++face) {
            for (int k = 0; k < 4; ++k) {
                const auto p = ps[selectVertex(face, k)];
                const auto idx = 4 * face + k;
                XMFLOAT2 uvs[] = {{p.z, p.y}, {p.z, p.x}, {p.x, p.y}};
                vs[idx] = Vertex{p, modifyColor(b.c, p), uvs[face % 3]};
            }
            const auto plusOffset = [o = 4 * face + Vertices.size()](uint16_t x) {
                return uint16_t(x + o);
            };
            const auto outIt = stdext::make_unchecked_array_iterator(is + 6 * face);
            if (face / 3)
                transform(begin(qis), end(qis), outIt, plusOffset);
            else
                transform(rbegin(qis), rend(qis), outIt, plusOffset);
        }

        Vertices.insert(end(Vertices), begin(vs), end(vs));
        Indices.insert(end(Indices), begin(is), end(is));
    }
};

struct Model {
    XMFLOAT3 Pos;
    ID3D11ShaderResourceViewPtr Tex;
    ID3D11BufferPtr VertexBuffer, IndexBuffer;
    size_t NumIndices;

    Model(ID3D11Device* device, const TriangleSet& t, XMFLOAT3 argPos,
          ID3D11ShaderResourceView* tex)
        : Pos(argPos), Tex(tex), NumIndices{size(t.Indices)} {
        auto byteSize = [](const auto& v) { return UINT(size(v) * sizeof(v[0])); };
        device->CreateBuffer(
            data({CD3D11_BUFFER_DESC{byteSize(t.Vertices), D3D11_BIND_VERTEX_BUFFER}}),
            data({D3D11_SUBRESOURCE_DATA{t.Vertices.data()}}), &VertexBuffer);

        device->CreateBuffer(
            data({CD3D11_BUFFER_DESC{byteSize(t.Indices), D3D11_BIND_INDEX_BUFFER}}),
            data({D3D11_SUBRESOURCE_DATA{t.Indices.data()}}), &IndexBuffer);
    }

    void Render(DirectX11& directx, const XMMATRIX& projView) const {
        const auto mat = XMMatrixTranslationFromVector(XMLoadFloat3(&Pos)) * projView;

        auto map = D3D11_MAPPED_SUBRESOURCE{};
        directx.Context->Map(directx.ConstantBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, &map);
        memcpy(map.pData, &mat, sizeof(mat));
        directx.Context->Unmap(directx.ConstantBuffer, 0);

        directx.Context->IASetInputLayout(directx.InputLayout);
        directx.Context->IASetIndexBuffer(IndexBuffer, DXGI_FORMAT_R16_UINT, 0);
        const auto vbs = {VertexBuffer.GetInterfacePtr()};
        directx.Context->IASetVertexBuffers(0, UINT(size(vbs)),
                                            data({VertexBuffer.GetInterfacePtr()}),
                                            data({UINT(sizeof(Vertex))}), data({0u}));
        directx.Context->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);

        directx.Context->VSSetShader(directx.D3DVert, nullptr, 0);
        directx.Context->PSSetShader(directx.D3DPix, nullptr, 0);

        const auto samplerStates = {directx.SamplerState.GetInterfacePtr()};
        directx.Context->PSSetSamplers(0, UINT(size(samplerStates)), data(samplerStates));

        const auto texSrvs = {Tex.GetInterfacePtr()};
        directx.Context->PSSetShaderResources(0, UINT(size(texSrvs)), data(texSrvs));
        directx.Context->DrawIndexed(UINT(NumIndices), 0, 0);
    }
};

struct Scene {
    vector<unique_ptr<Model>> Models;

    void Render(DirectX11& directx, const XMMATRIX& projView) const {
        for (const auto& model : Models) model->Render(directx, projView);
    }

    Scene(ID3D11Device* device, ID3D11DeviceContext* context) {
        auto add = [this, device](auto&&... xs) {
            Models.push_back(make_unique<Model>(device, xs...));
        };

        enum : uint32_t {
            DARKER_GREY = 0xff383838,
            DARK_GREY = 0xff404040,
            MID_DARK_GREY = 0xff505050,
            MID_GREY = 0xff808080,
            RED = 0xffff0000,
            DARK_YELLOW = 0xff505000,
            DARK_BLUE = 0xff202050
        };

        TriangleSet cube{{0.5f, -0.5f, 0.5f, -0.5f, 0.5f, -0.5f, DARK_GREY}};
        add(cube, XMFLOAT3{0, 0, 0}, createTexture(device, context, TextureFill::AUTO_CEILING));

        TriangleSet spareCube{{0.1f, -0.1f, 0.1f, -0.1f, +0.1f, -0.1f, RED}};
        add(spareCube, XMFLOAT3{0, -10, 0},
            createTexture(device, context, TextureFill::AUTO_CEILING));

        TriangleSet walls{{10.1f, 0.0f, 20.0f, 10.0f, 4.0f, -20.0f, MID_GREY},      // Left Wall
                          {10.0f, -0.1f, 20.1f, -10.0f, 4.0f, 20.0f, MID_GREY},     // Back Wall
                          {-10.0f, -0.1f, 20.0f, -10.1f, 4.0f, -20.0f, MID_GREY}};  // Right Wall
        add(walls, XMFLOAT3{0, 0, 0}, createTexture(device, context, TextureFill::AUTO_WALL));

        TriangleSet floors{
            {10.0f, -0.1f, 20.0f, -10.0f, 0.0f, -20.1f, MID_GREY},     // Main floor
            {15.0f, -6.1f, -18.0f, -15.0f, -6.0f, -30.0f, MID_GREY}};  // Bottom floor
        add(floors, XMFLOAT3{0, 0, 0},
            createTexture(device, context, TextureFill::AUTO_FLOOR));  // Floors

        TriangleSet ceiling{{10.0f, 4.0f, 20.0f, -10.0f, 4.1f, -20.1f, MID_GREY}};
        add(ceiling, XMFLOAT3{0, 0, 0},
            createTexture(device, context, TextureFill::AUTO_CEILING));  // Ceiling

        TriangleSet furniture{
            {-9.5f, 0.75f, -3.0f, -10.1f, 2.5f, -3.1f, DARKER_GREY},   // Right side shelf verticals
            {-9.5f, 0.95f, -3.7f, -10.1f, 2.75f, -3.8f, DARKER_GREY},  // Right side shelf
            {-9.55f, 1.20f, -2.5f, -10.1f, 1.30f, -3.75f, DARKER_GREY},  // Right side shelf horiz
            {-9.55f, 2.00f, -3.05f, -10.1f, 2.10f, -4.2f, DARKER_GREY},  // Right side shelf
            {-5.0f, 1.1f, -20.0f, -10.0f, 1.2f, -20.1f, DARKER_GREY},    // Right railing
            {10.0f, 1.1f, -20.0f, 5.0f, 1.2f, -20.1f, DARKER_GREY},      // Left railing
            {1.8f, 0.8f, -1.0f, 0.0f, 0.7f, 0.0f, DARK_YELLOW},          // Table
            {1.8f, 0.0f, 0.0f, 1.7f, 0.7f, -0.1f, DARK_YELLOW},          // Table Leg
            {1.8f, 0.7f, -1.0f, 1.7f, 0.0f, -0.9f, DARK_YELLOW},         // Table Leg
            {0.0f, 0.0f, -1.0f, 0.1f, 0.7f, -0.9f, DARK_YELLOW},         // Table Leg
            {0.0f, 0.7f, 0.0f, 0.1f, 0.0f, -0.1f, DARK_YELLOW},          // Table Leg
            {1.4f, 0.5f, 1.1f, 0.8f, 0.55f, 0.5f, DARK_BLUE},            // Chair Set
            {1.401f, 0.0f, 1.101f, 1.339f, 1.0f, 1.039f, DARK_BLUE},     // Chair Leg 1
            {1.401f, 0.5f, 0.499f, 1.339f, 0.0f, 0.561f, DARK_BLUE},     // Chair Leg 2
            {0.799f, 0.0f, 0.499f, 0.861f, 0.5f, 0.561f, DARK_BLUE},     // Chair Leg 2
            {0.799f, 1.0f, 1.101f, 0.861f, 0.0f, 1.039f, DARK_BLUE},     // Chair Leg 2
            {1.4f, 0.97f, 1.05f, 0.8f, 0.92f, 1.10f, DARK_BLUE}};        // Chair Back high bar
        for (float f = 5; f <= 9; f += 1)
            furniture.AddBox(
                {-f, 0.0f, -20.0f, -f - 0.1f, 1.1f, -20.1f, MID_DARK_GREY});  // Left Bars
        for (float f = 5; f <= 9; f += 1)
            furniture.AddBox(
                {f, 1.1f, -20.0f, f + 0.1f, 0.0f, -20.1f, MID_DARK_GREY});  // Right Bars
        for (float f = 3.0f; f <= 6.6f; f += 0.4f)
            furniture.AddBox({3, 0.0f, -f, 2.9f, 1.3f, -f - 0.1f, DARK_GREY});  // Posts
        add(furniture, XMFLOAT3{0, 0, 0},
            createTexture(device, context, TextureFill::AUTO_WHITE));  // Fixtures & furniture
    }
};

struct Camera {
    XMVECTOR Pos, Rot;
    auto GetViewMatrix() const {
        const auto forward = XMVector3Rotate(XMVectorSet(0, 0, -1, 0), Rot),
                   up = XMVector3Rotate(XMVectorSet(0, 1, 0, 0), Rot);
        return XMMatrixLookAtRH(Pos, Pos + forward, up);
    }
};

// ovrSwapTextureSet wrapper class that also maintains the render target views needed for D3D11
// rendering.
struct OculusTexture {
    unique_ptr<ovrSwapTextureSet, function<void(ovrSwapTextureSet*)>> TextureSet;
    ID3D11RenderTargetViewPtr TexRtvs[2];

    OculusTexture(ID3D11Device* device, ovrSession session, ovrSizei size)
        : TextureSet{[session, size, device, &texRtv = TexRtvs] {
                         // Create and validate the swap texture set and stash it in unique_ptr
                         ovrSwapTextureSet* ts{};
                         VALIDATE(OVR_SUCCESS(ovr_CreateSwapTextureSetD3D11(
                                      session, device,
                                      data({CD3D11_TEXTURE2D_DESC(
                                          DXGI_FORMAT_R8G8B8A8_UNORM_SRGB, size.w, size.h, 1, 1,
                                          D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET)}),
                                      ovrSwapTextureSetD3D11_Typeless, &ts)),
                                  "Failed to create SwapTextureSet.");
                         VALIDATE(size_t(ts->TextureCount) == std::size(texRtv),
                                  "TextureCount mismatch.");
                         return ts;
                     }(),
                     // unique_ptr Deleter lambda to clean up the swap texture set
                     [session](ovrSwapTextureSet* ts) { ovr_DestroySwapTextureSet(session, ts); }} {
        // Create render target views for each of the textures in the swap texture set
        transform(TextureSet->Textures, TextureSet->Textures + TextureSet->TextureCount, TexRtvs,
                  [device](auto tex) {
                      ID3D11RenderTargetViewPtr rtv;
                      device->CreateRenderTargetView(
                          reinterpret_cast<ovrD3D11Texture&>(tex).D3D11.pTexture,
                          data({CD3D11_RENDER_TARGET_VIEW_DESC{D3D11_RTV_DIMENSION_TEXTURE2D,
                                                               DXGI_FORMAT_R8G8B8A8_UNORM}}),
                          &rtv);
                      return rtv;
                  });
    }

    auto AdvanceToNextTexture() {
        return TextureSet->CurrentIndex = (TextureSet->CurrentIndex + 1) % TextureSet->TextureCount;
    }
};

DirectX11::DirectX11(HWND window, int vpW, int vpH, const LUID* pLuid)
    : WinSizeW{vpW}, WinSizeH{vpH} {
    auto windowSize = RECT{0, 0, WinSizeW, WinSizeH};
    AdjustWindowRect(&windowSize, WS_OVERLAPPEDWINDOW, false);
    SetWindowPos(window, nullptr, 0, 0, windowSize.right - windowSize.left,
                 windowSize.bottom - windowSize.top, SWP_NOMOVE | SWP_NOZORDER | SWP_SHOWWINDOW);

    IDXGIFactoryPtr dxgiFactory;
    VALIDATE(
        SUCCEEDED(CreateDXGIFactory1(dxgiFactory.GetIID(), reinterpret_cast<void**>(&dxgiFactory))),
        "CreateDXGIFactory1 failed");

    IDXGIAdapterPtr adapter;
    for (int iAdapter = 0; dxgiFactory->EnumAdapters(iAdapter, &adapter) != DXGI_ERROR_NOT_FOUND;
         ++iAdapter) {
        DXGI_ADAPTER_DESC adapterDesc{};
        adapter->GetDesc(&adapterDesc);
        if (!pLuid|| memcmp(&adapterDesc.AdapterLuid, pLuid, sizeof(LUID)) == 0) break;
    }

    const auto DriverType = adapter ? D3D_DRIVER_TYPE_UNKNOWN : D3D_DRIVER_TYPE_HARDWARE;
    const auto createFlags = [] {
#ifdef _DEBUG
        return D3D11_CREATE_DEVICE_DEBUG;
#else
        return D3D11_CREATE_DEVICE_FLAG(0);
#endif
    }();
    VALIDATE(SUCCEEDED(D3D11CreateDeviceAndSwapChain(
                 adapter, DriverType, nullptr, createFlags, nullptr, 0, D3D11_SDK_VERSION,
                 data({DXGI_SWAP_CHAIN_DESC{
                     {UINT(WinSizeW), UINT(WinSizeH), {}, DXGI_FORMAT_R8G8B8A8_UNORM},  // Buffer
                     {1},  // SampleDesc
                     DXGI_USAGE_RENDER_TARGET_OUTPUT,
                     2,  // BufferCount
                     window,
                     TRUE,
                     DXGI_SWAP_EFFECT_SEQUENTIAL}}),
                 &SwapChain, &Device, nullptr, &Context)),
             "D3D11CreateDeviceAndSwapChain failed");

    // Create backbuffer
    VALIDATE(SUCCEEDED(SwapChain->GetBuffer(0, BackBuffer.GetIID(),
                                            reinterpret_cast<void**>(&BackBuffer))),
             "IDXGISwapChain::GetBuffer() failed");

    // Buffer for shader constants
    Device->CreateBuffer(data({CD3D11_BUFFER_DESC(sizeof(XMMATRIX), D3D11_BIND_CONSTANT_BUFFER,
                                                  D3D11_USAGE_DYNAMIC, D3D11_CPU_ACCESS_WRITE)}),
                         nullptr, &ConstantBuffer);
    auto buffs = {ConstantBuffer.GetInterfacePtr()};
    Context->VSSetConstantBuffers(0, UINT(size(buffs)), data(buffs));

    // Set max frame latency to 1
    IDXGIDevice1Ptr DXGIDevice1;
    VALIDATE(SUCCEEDED(Device.QueryInterface(DXGIDevice1.GetIID(), &DXGIDevice1)),
             "QueryInterface failed");
    DXGIDevice1->SetMaximumFrameLatency(1);

    // Set up render states
    // Create and set rasterizer state
    ID3D11RasterizerStatePtr rss;
    Device->CreateRasterizerState(data({CD3D11_RASTERIZER_DESC{D3D11_DEFAULT}}), &rss);
    Context->RSSetState(rss);

    // Create and set depth stencil state
    ID3D11DepthStencilStatePtr dss;
    Device->CreateDepthStencilState(data({CD3D11_DEPTH_STENCIL_DESC{D3D11_DEFAULT}}), &dss);
    Context->OMSetDepthStencilState(dss, 0);

    // Create and set blend state
    ID3D11BlendStatePtr bs;
    Device->CreateBlendState(data({CD3D11_BLEND_DESC{D3D11_DEFAULT}}), &bs);
    Context->OMSetBlendState(bs, nullptr, 0xffffffff);

    auto compileShader = [](const char* src, const char* target) {
        ID3DBlobPtr blob;
        D3DCompile(src, strlen(src), nullptr, nullptr, nullptr, "main", target, 0, 0, &blob,
                   nullptr);
        return blob;
    };

    // Create vertex shader and input layout
    auto defaultVertexShaderSrc = R"(float4x4 ProjView;
                                         void main(in float4 pos : POSITION,
                                                   in float4 col : COLOR0,
                                                   in float2 tex : TEXCOORD0,
                                                   out float4 oPos : SV_Position,
                                                   out float4 oCol : COLOR0,
                                                   out float2 oTex : TEXCOORD0) {
                                             oPos = mul(ProjView, pos);
                                             oTex = tex;
                                             oCol = col;
                                         })";
    auto vsBlob = compileShader(defaultVertexShaderSrc, "vs_4_0");
    Device->CreateVertexShader(vsBlob->GetBufferPointer(), vsBlob->GetBufferSize(), nullptr,
                               &D3DVert);
    D3D11_INPUT_ELEMENT_DESC defaultVertexDesc[] = {
        {"Position", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, offsetof(Vertex, pos)},
        {"Color", 0, DXGI_FORMAT_B8G8R8A8_UNORM, 0, offsetof(Vertex, c)},
        {"TexCoord", 0, DXGI_FORMAT_R32G32_FLOAT, 0, offsetof(Vertex, uv)},
    };
    Device->CreateInputLayout(defaultVertexDesc, UINT(size(defaultVertexDesc)),
                              vsBlob->GetBufferPointer(), vsBlob->GetBufferSize(), &InputLayout);

    // Create pixel shader
    auto defaultPixelShaderSrc = R"(Texture2D Texture : register(t0);
                                        SamplerState Linear : register(s0);
                                        float4 main(in float4 Position : SV_Position,
                                                    in float4 Color: COLOR0,
                                                    in float2  TexCoord : TEXCOORD0) : SV_Target {
                                            float4 TexCol = Texture.Sample(Linear, TexCoord);
                                            return(Color * TexCol);
                                        })";
    auto psBlob = compileShader(defaultPixelShaderSrc, "ps_4_0");
    Device->CreatePixelShader(psBlob->GetBufferPointer(), psBlob->GetBufferSize(), nullptr,
                              &D3DPix);

    // Create sampler state
    auto ss = CD3D11_SAMPLER_DESC{D3D11_DEFAULT};
    ss.Filter = D3D11_FILTER_ANISOTROPIC;
    ss.AddressU = ss.AddressV = ss.AddressW = D3D11_TEXTURE_ADDRESS_WRAP;
    ss.MaxAnisotropy = 8;
    Device->CreateSamplerState(&ss, &SamplerState);
}

// Helper to wrap ovr types like ovrSession and ovrTexture* in a unique_ptr with custom create / destroy
auto create_unique = [](auto createFunc, auto destroyFunc) {
    return unique_ptr<remove_reference_t<decltype(*createFunc())>, decltype(destroyFunc)>{
        createFunc(), destroyFunc};
};

ovrResult MainLoop(const Window& window) {
    auto result = ovrResult{};
    auto luid = ovrGraphicsLuid{};
    // Initialize the session, stash it in a unique_ptr for automatic cleanup.
    auto session = create_unique(
        [&result, &luid] {
            ovrSession session{};
            result = ovr_Create(&session, &luid);
            return session;
        },
        ovr_Destroy);
    if (OVR_FAILURE(result)) return result;

    auto hmdDesc = ovr_GetHmdDesc(session.get());

    // Setup Device and shared D3D objects (shaders, state objects, etc.)
    // Note: the mirror window can be any size, for this sample we use 1/2 the HMD resolution
    auto directx = DirectX11{window.Hwnd, hmdDesc.Resolution.w / 2, hmdDesc.Resolution.h / 2,
                             reinterpret_cast<LUID*>(&luid)};

    // Create the eye render buffers (caution if actual size < requested due to HW limits).
    const ovrSizei idealSizes[] = {
        ovr_GetFovTextureSize(session.get(), ovrEye_Left, hmdDesc.DefaultEyeFov[ovrEye_Left], 1.0f),
        ovr_GetFovTextureSize(session.get(), ovrEye_Right, hmdDesc.DefaultEyeFov[ovrEye_Right], 1.0f)};
    OculusTexture eyeRenderTextures[] = {{directx.Device, session.get(), idealSizes[ovrEye_Left]},
                                         {directx.Device, session.get(), idealSizes[ovrEye_Right]}};
    DepthBuffer eyeDepthBuffers[] = {{directx.Device, idealSizes[ovrEye_Left]},
                                     {directx.Device, idealSizes[ovrEye_Right]}};
    const ovrRecti eyeRenderViewports[] = {{{0, 0}, idealSizes[ovrEye_Left]},
                                           {{0, 0}, idealSizes[ovrEye_Right]}};

    // Create mirror texture to see on the monitor, stash it in a unique_ptr for automatic cleanup.
    auto mirrorTexture = create_unique(
        [&result, session = session.get(), &directx] {
            ovrTexture* mirrorTexture{};
            result = ovr_CreateMirrorTextureD3D11(
                session, directx.Device,
                data({CD3D11_TEXTURE2D_DESC(DXGI_FORMAT_R8G8B8A8_UNORM_SRGB, directx.WinSizeW,
                                            directx.WinSizeH, 1, 1)}),
                0, &mirrorTexture);
            return mirrorTexture;
        },
        // lambda to destroy mirror texture on unique_ptr destruction
        [session = session.get()](ovrTexture* mt) { ovr_DestroyMirrorTexture(session, mt); });
    if (OVR_FAILURE(result)) return result;

    // Initialize the scene and camera
    auto roomScene = Scene{directx.Device, directx.Context};
    auto mainCam = Camera{XMVectorSet(0.0f, 1.6f, 5.0f, 0), XMQuaternionIdentity()};

    // Main loop
    while (window.HandleMessages()) {
        // Handle input
        [&mainCam, &window] {
            const auto forward = XMVector3Rotate(XMVectorSet(0, 0, -0.05f, 0), mainCam.Rot);
            const auto right = XMVector3Rotate(XMVectorSet(0.05f, 0, 0, 0), mainCam.Rot);
            if (window.Keys['W'] || window.Keys[VK_UP]) mainCam.Pos += forward;
            if (window.Keys['S'] || window.Keys[VK_DOWN]) mainCam.Pos -= forward;
            if (window.Keys['D']) mainCam.Pos += right;
            if (window.Keys['A']) mainCam.Pos -= right;
            static auto Yaw = 0.0f;
            if (window.Keys[VK_LEFT])
                mainCam.Rot = XMQuaternionRotationRollPitchYaw(0, Yaw += 0.02f, 0);
            if (window.Keys[VK_RIGHT])
                mainCam.Rot = XMQuaternionRotationRollPitchYaw(0, Yaw -= 0.02f, 0);
        }();

        // Animate the cube
        [&cube = roomScene.Models[0]] {
            static auto cubeClock = 0.0f;
            cube->Pos = {9 * sin(cubeClock), 3, 9 * cos(cubeClock += 0.015f)};
        }();

        // Get both eye poses simultaneously, with IPD offset already included.
        const ovrEyeRenderDesc eyeRenderDesc[] = {
            ovr_GetRenderDesc(session.get(), ovrEye_Left, hmdDesc.DefaultEyeFov[ovrEye_Left]),
            ovr_GetRenderDesc(session.get(), ovrEye_Right, hmdDesc.DefaultEyeFov[ovrEye_Right])};

        const auto sensorSampleTime = ovr_GetTimeInSeconds();
        const auto eyeRenderPoses = [session = session.get(), &eyeRenderDesc] {
            array<ovrPosef, 2> res;
            const auto frameTime = ovr_GetPredictedDisplayTime(session, 0);
            // Keeping sensorSampleTime as close to ovr_GetTrackingState as possible - fed into the
            // layer
            const auto hmdState = ovr_GetTrackingState(session, frameTime, ovrTrue);
            const ovrVector3f HmdToEyeViewOffset[] = {
                eyeRenderDesc[ovrEye_Left].HmdToEyeViewOffset,
                eyeRenderDesc[ovrEye_Right].HmdToEyeViewOffset};
            ovr_CalcEyePoses(hmdState.HeadPose.ThePose, HmdToEyeViewOffset, res.data());
            return res;
        }();

        // Render Scene to Eye Buffers
        for (auto eye : {ovrEye_Left, ovrEye_Right}) {
            // Increment to use next texture, just before rendering
            const auto texIndex = eyeRenderTextures[eye].AdvanceToNextTexture();
            directx.SetAndClearRenderTarget(eyeRenderTextures[eye].TexRtvs[texIndex],
                                            &eyeDepthBuffers[eye]);
            directx.SetViewport(eyeRenderViewports[eye]);

            // Get the pose information in XM format
            const auto& ori = eyeRenderPoses[eye].Orientation;
            const auto eyeQuat = XMVectorSet(ori.x, ori.y, ori.z, ori.w);
            const auto& pos = eyeRenderPoses[eye].Position;
            const auto eyePos = XMVectorSet(pos.x, pos.y, pos.z, 0);

            // Get view and projection matrices for the eye camera
            const auto CombinedPos = mainCam.Pos + XMVector3Rotate(eyePos, mainCam.Rot);
            const auto finalCam = Camera{CombinedPos, XMQuaternionMultiply(eyeQuat, mainCam.Rot)};
            const auto p = ovrMatrix4f_Projection(eyeRenderDesc[eye].Fov, 0.2f, 1000.0f,
                                                    ovrProjection_RightHanded);
            const auto proj = XMMatrixTranspose(XMMATRIX{&p.M[0][0]});

            // Render the scene
            roomScene.Render(directx, finalCam.GetViewMatrix() * proj);
        }

        // Initialize our single full screen Fov layer.
        const auto ld = [&eyeRenderTextures, &eyeRenderViewports, &hmdDesc, &eyeRenderPoses,
                         sensorSampleTime] {
            auto res = ovrLayerEyeFov{{ovrLayerType_EyeFov, 0}};
            for (auto eye : {ovrEye_Left, ovrEye_Right}) {
                res.ColorTexture[eye] = eyeRenderTextures[eye].TextureSet.get();
                res.Viewport[eye] = eyeRenderViewports[eye];
                res.Fov[eye] = hmdDesc.DefaultEyeFov[eye];
                res.RenderPose[eye] = eyeRenderPoses[eye];
                res.SensorSampleTime = sensorSampleTime;
            }
            return res;
        }();
        const auto layers = &ld.Header;
        result = ovr_SubmitFrame(session.get(), 0, nullptr, &layers, 1);
        // exit the rendering loop if submit returns an error, will retry on ovrError_DisplayLost
        if (OVR_FAILURE(result)) return result;

        // Display mirror texture on monitor
        directx.Context->CopyResource(
            directx.BackBuffer,
            reinterpret_cast<ovrD3D11Texture*>(mirrorTexture.get())->D3D11.pTexture);
        directx.SwapChain->Present(0, 0);
    }

    return result;
}

//-------------------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE hinst, HINSTANCE, LPSTR, int) {
    // Initializes LibOVR, and the Rift
    VALIDATE(OVR_SUCCESS(ovr_Initialize(nullptr)), "Failed to initialize libOVR.");

    Window window{hinst, L"Oculus Room Tiny (DX11)"};
    window.Run(MainLoop);

    ovr_Shutdown();
    return 0;
}
