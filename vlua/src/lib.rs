use mlua::prelude::*;
use vcore::{ CPU, Word };
use mlua::{UserData, UserDataMethods};
use vcore::memory::{PeekPoke, PeekPokeExt};
use std::iter::FromIterator;

#[mlua::lua_module]
fn libvlua(lua: &Lua) -> LuaResult<LuaTable> {
    let exports = lua.create_table()?;
    let cpu_constructor = lua.create_function(|_, _: ()| Ok(LuaCPU(CPU::new_random())))?;
    exports.set("new", cpu_constructor)?;
    Ok(exports)
}

struct LuaCPU(CPU);

impl UserData for LuaCPU {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method_mut("reset", |_, lcpu, ()| {
            lcpu.0.reset();
            Ok(())
        });

        methods.add_method_mut("push_data", |_, lcpu, num: i32| {
            lcpu.0.push_data(Word::from(num));
            Ok(())
        });

        methods.add_method_mut("pop_data", |_, lcpu, ()| {
            let d = lcpu.0.pop_data();
            Ok(u32::from(d))
        });

        methods.add_method_mut("push_call", |_, lcpu, num: i32| {
            lcpu.0.push_call(Word::from(num));
            Ok(())
        });

        methods.add_method_mut("pop_call", |_, lcpu, ()| {
            let d = lcpu.0.pop_call();
            Ok(u32::from(d))
        });

        methods.add_method_mut("poke", |_, lcpu, (addr, val): (u32, u8)| {
            lcpu.0.poke(Word::from(addr), val);
            Ok(())
        });

        methods.add_method("peek", |_, lcpu, addr: u32| {
            let b = lcpu.0.peek(Word::from(addr));
            Ok(b)
        });

        methods.add_method_mut("poke24", |_, lcpu, (addr, val): (u32, i32)| {
            lcpu.0.poke24(Word::from(addr), Word::from(val));
            Ok(())
        });

        methods.add_method("peek24", |_, lcpu, addr: u32| {
            let b = lcpu.0.peek24(Word::from(addr));
            Ok(u32::from(b))
        });

        methods.add_method("pc", |_, lcpu, ()| {
            Ok(u32::from(lcpu.0.pc()))
        });

        methods.add_method("sp", |_, lcpu, ()| {
            Ok(u32::from(lcpu.0.sp()))
        });

        methods.add_method("dp", |_, lcpu, ()| {
            Ok(u32::from(lcpu.0.dp()))
        });

        methods.add_method_mut("set_pc", |_, lcpu, val: u32| {
            lcpu.0.set_pc(Word::from(val));
            Ok(())
        });

        methods.add_method("print_stack", |_, lcpu, ()| {
            let (btm, dp) = (lcpu.0.dp_btm(), lcpu.0.dp());
            if dp == btm {
                println!("<stack empty>")
            } else {
                let mut curr = btm;
                while curr < dp {
                    let w = lcpu.0.peek24(curr);
                    println!("{:#08x}:\t{:#08x} {}", u32::from(curr), u32::from(w), i32::from(w));
                    curr += 3;
                }
            }
            Ok(())
        });

        methods.add_method("print_r_stack", |_, lcpu, ()| {
            let (top, sp) = (lcpu.0.sp_top(), lcpu.0.sp());
            if sp == top {
                println!("<stack empty>")
            } else {
                let mut curr = top - 3;
                while curr >= sp {
                    let w = lcpu.0.peek24(curr);
                    println!("{:#08x}:\t{:#08x} {}", u32::from(curr), u32::from(w), i32::from(w));
                    curr -= 3;
                }
            }
            Ok(())
        });

        methods.add_method("stack", |_, lcpu, ()| {
            Ok(Vec::from_iter(lcpu.0.get_stack().into_iter().map(|w| u32::from(w))))
        });

        methods.add_method("r_stack", |_, lcpu, ()| {
            Ok(Vec::from_iter(lcpu.0.get_call().into_iter().map(|w| u32::from(w))))
        });

        methods.add_method_mut("run", |_, lcpu, ()| {
            lcpu.0.run_to_halt();
            Ok(())
        });

        methods.add_method("flags", |_, lcpu, ()| {
            Ok((lcpu.0.halted(), lcpu.0.int_enabled()))
        });

        methods.add_method_mut("interrupt", |_, lcpu, (irq, arg): (u8, Option<i32>)| {
            lcpu.0.interrupt(irq as usize, arg.map(|n| Word::from(n)));
            Ok(())
        });
    }
}
